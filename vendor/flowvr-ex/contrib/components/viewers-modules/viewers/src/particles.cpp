/******* COPYRIGHT ************************************************
*                                                                 *
*                         FlowVR Render                           *
*                   Parallel Rendering Modules                    *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA                                                           *
*  ALL RIGHTS RESERVED.                                           *
*                                                                 *
* This source is covered by the GNU GPL, please refer to the      *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Clement Menier.                                              *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: src/viewers/particles.cpp                                 *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#include <flowvr/module.h>
#include <flowvr/render/chunkwriter.h>
#include <ftl/cmdline.h>

#include <math.h>
#include <stdlib.h>

// In order not to type flowvr::render everywhere, include the whole namespace
using namespace flowvr::render;

ftl::Option<int> optOrder("order",'o',"rendering order",10);
ftl::Option<std::string> optTexture("texture",'t',"texture used for the particles");
ftl::Option<float> optSize("size",'s',"particle size in pixels",10.0f);
ftl::FlagOption optBlend("blend",'b',"activate blending");
ftl::Option<float> optAlpha("alpha",'a',"particle alpha",1.0f);
ftl::Option<std::string> optVShader("vshader",'v',"vertex shader");
ftl::Option<std::string> optPShader("pshader",'p',"pixel shader");

int main(int argc, char** argv)
{
  ftl::CmdLine cmd("Particles viewer");
  bool error=false;
  if (!cmd.parse(argc,argv,&error))
    return error?1:0;

  // Declare a FlowVR Render output port to send our scene description.
  SceneOutputPort pOut("scene");

  // Data input ports

  flowvr::InputPort pInPoints("points");
  flowvr::InputPort pInColors("colors");

  // Initialize FlowVR
  std::vector<flowvr::Port*> ports;
  ports.push_back(&pInPoints);
  ports.push_back(&pInColors);
  ports.push_back(&pOut);
  flowvr::ModuleAPI* module = flowvr::initModule(ports);
  if (module == NULL)
    return 1;

  // Helper class to construct primitives update chunks
  flowvr::render::ChunkRenderWriter scene;

  // Get IDs for all our primitives and resources
  ID idPrim = module->generateID();
  ID idTexture = module->generateID();
  ID idVB = module->generateID();
  ID idIB = module->generateID();
  ID idVS = module->generateID();
  ID idPS = module->generateID();

  if (optVShader.count>0)
  {
    scene.loadVertexShader(idVS, optVShader);
  }
  else
  {
    scene.loadVertexShader(idVS, "shaders/sprite_v.cg");
  }
  if (optPShader.count>0)
  {
    scene.loadPixelShader(idPS, optPShader);
  }
  else if (optTexture.count>0)
  {
    scene.loadPixelShader(idPS, "shaders/sprite_p.cg");
  }
  else
  {
    scene.loadPixelShader(idPS, "shaders/color_p.cg");
  }
  // Load texture
  if (optTexture.count>0)
  {
    scene.loadTexture(idTexture,((std::string)optTexture).c_str());
  }

  // Create new primitive
  scene.addPrimitive(idPrim, "Points");

  // Set shaders
  scene.addParamID(idPrim, ChunkPrimParam::VSHADER,"",idVS);
  scene.addParamID(idPrim, ChunkPrimParam::PSHADER,"",idPS);

  // Set shaders parameters
  scene.addParamEnum(idPrim, ChunkPrimParam::PARAMVSHADER, "ModelViewProj", ChunkPrimParam::ModelViewProjection);
  scene.addParamEnum(idPrim, ChunkPrimParam::PARAMVSHADER, "ModelView", ChunkPrimParam::ModelView);
  scene.addParamEnum(idPrim, ChunkPrimParam::PARAMVSHADER, "ModelViewIT", ChunkPrimParam::ModelView_InvTrans);
  scene.addParam(idPrim, ChunkPrimParam::PARAMVSHADER, "size", (float)optSize);
  if (optTexture.count>0)
    scene.addParamID(idPrim, ChunkPrimParam::TEXTURE, "texture", idTexture);
  scene.addParam(idPrim,ChunkPrimParam::ORDER,"",(int)optOrder);
  if (optBlend)
  {
    scene.addParam(idPrim,ChunkPrimParam::PARAMOPENGL,"Blend",true);
    if (optBlend.count==2)
      scene.addParamEnum(idPrim,ChunkPrimParam::PARAMOPENGL,"BlendMode",BLEND_ADD);
    else if (optBlend.count==3)
      scene.addParamEnum(idPrim,ChunkPrimParam::PARAMOPENGL,"BlendMode",BLEND_PREMULT);
    scene.addParam(idPrim,ChunkPrimParam::PARAMOPENGL,"DepthWrite",false);
  }

  // Set index buffer
  // No index (display all points sequentially), just specify type of GL primitive
  scene.addIndexBuffer(idIB, 0, Type::Null, ChunkIndexBuffer::Point);
  scene.addParamID(idPrim, ChunkPrimParam::IBUFFER_ID, "", idIB);


  // Main FlowVR loop. Contains the animations of the scene
  int it=0;
  while (module->wait())
  {
    // Update scene

    // Read positions
    flowvr::Message msg;
    module->get(&pInPoints, msg);
    const Vec3f* src = msg.data.getRead<Vec3f>();
    int nbp = msg.data.getSize()/sizeof(Vec3f);

    // Compute bounding box
    BBox bb;
    for (int p=0;p<nbp;p++)
      bb += src[p];

    // Read colors
    flowvr::Message msgcolors;
    module->get(&pInColors, msgcolors);
    if (msgcolors.valid() && msgcolors.data.getSize()>=nbp*(int)(sizeof(Vec3f)))
    {
      // Send positions and colors
      const Vec3f* colors = msgcolors.data.getRead<Vec3f>();
      if (optAlpha)
      {
	int dataTypes[2] = { Type::Vec3f, Type::Vec4f };
	ChunkVertexBuffer* vb = scene.addVertexBuffer(idVB, nbp, 2, dataTypes, bb);
	vb->gen = it; // specify data update number
	Vec3f* dest = (Vec3f*) vb->data();
	
	for (int p=0;p<nbp;p++)
	{
	  *(dest++) = *(src++);
	  *(dest++) = Vec4f(*(colors++),(float)optAlpha);
	}
      }
      else
      {
	int dataTypes[2] = { Type::Vec3f, Type::Vec3f };
	ChunkVertexBuffer* vb = scene.addVertexBuffer(idVB, nbp, 2, dataTypes, bb);
	vb->gen = it; // specify data update number
	Vec3f* dest = (Vec3f*) vb->data();
	
	for (int p=0;p<nbp;p++)
	{
	  *(dest++) = *(src++);
	  *(dest++) = *(colors++);
	}
      }

      // Link vertex attributes in primitive
      scene.addParamID(idPrim, ChunkPrimParam::VBUFFER_ID, "position", idVB);
      scene.addParamID(idPrim, ChunkPrimParam::VBUFFER_ID, "color", idVB);
      scene.addParam(idPrim, ChunkPrimParam::VBUFFER_NUMDATA, "color", int(1));
    }
    else
    {
      // Send positions
      int dataTypes[1] = { Type::Vec3f };
      ChunkVertexBuffer* vb = scene.addVertexBuffer(idVB, nbp, 1, dataTypes, bb);
      vb->gen = it; // specify data update number
      Vec3f* dest = (Vec3f*) vb->data();
      memcpy(dest, src, vb->dataSize());

      // Link vertex attributes in primitive
      scene.addParamID(idPrim, ChunkPrimParam::VBUFFER_ID, "position", idVB);
    }

    // Set the number of points in the primitive
    scene.addParam(idPrim, ChunkPrimParam::IBUFFER_NBI,"",nbp);

    // Send message
    scene.put(&pOut);

    ++it;
  }

  module->close();

  return 0;
}
