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
* File: src/viewers/libflowvr-opengl-wrapper.cpp                  *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
/********************************************************************************
 * GL - OpenGL/GLX buffer readback for flowvr-render
 * based on sage's OpenGL Wrapper
 *********************************************************************************/

#include <flowvr/module.h>
#include <flowvr/render/chunkwriter.h>

#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/Xatom.h>


#if defined(__APPLE__)
#include <OpenGL/glu.h>
#else
#include <GL/glu.h>
#endif
#include <GL/glx.h>



using namespace flowvr::render;
using flowvr::ID;

static int winWidth = -1, winHeight = -1;

typedef void (*glXSwapBuffersFunc_t) (Display*, GLXDrawable);
typedef void (*glViewporFunc_t) (GLint, GLint, GLsizei, GLsizei);

static glXSwapBuffersFunc_t funcswap = NULL;
static glViewporFunc_t funcviewport = NULL;


/* FlowVR setup part */
namespace {
  flowvr::ModuleAPI *flowvr_module = NULL;
  ChunkRenderWriter scene;
  SceneOutputPort *flowvr_scene_port = NULL;
  ID idPrim, idT, idVB, idIB, idVS, idPS, idPS_R;
  int gen=0;
  int dataType[2] = { Type::Vec3f, Type::Vec2f };
  struct Vertex
  {
    Vec3f pos;
    Vec2f uv;
  };
  bool first = true;
  
  bool isPow2(int x)
  {
    return (x&(x-1))==0;
  }


}
void InitTrap()
{
  void *result;
  void* handle;
  std::cerr << "Trapping..." << std::endl;
  // Init FlowVR
  std::vector<flowvr::Port*> ports;
  flowvr_scene_port = new SceneOutputPort("scene");
  ports.push_back(flowvr_scene_port);
  flowvr_module = flowvr::initModule(ports);
  if (!flowvr_module) {
    std::cerr << "Unable to setup FlowVR up. Exiting..." << std::endl;
    exit(1);
  }

  /* Generate IDs */
  idPrim = flowvr_module->generateID();
  idT = flowvr_module->generateID();
  idVB = flowvr_module->generateID();
  idIB = flowvr_module->generateID();
  idPS = flowvr_module->generateID();
  idPS_R = flowvr_module->generateID();
  idVS = flowvr_module->generateID();
    
  ChunkTexture *texture = scene.addDefaultTexture(idT);
  texture->level=0;
  texture->gen=gen;
    
  gen++;

  ChunkVertexBuffer *vb = scene.addVertexBuffer(idVB, 4,2, dataType);
  vb->gen=gen;
  Vertex *vertex = (Vertex*)vb->data();
    
  vertex[0].pos = Vec3f(-1, 1,0); vertex[0].uv = Vec2f(0,1);
  vertex[1].pos = Vec3f( 1, 1,0); vertex[1].uv = Vec2f(1,1);
  vertex[2].pos = Vec3f( 1,-1,0); vertex[2].uv = Vec2f(1,0);
  vertex[3].pos = Vec3f(-1,-1,0); vertex[3].uv = Vec2f(0,0);

  ChunkIndexBuffer* ib = scene.addIndexBuffer(idIB, 4, Type::Byte, ChunkIndexBuffer::Quad);
  unsigned char* ind = (unsigned char*)ib->data();
  ind[0] = 0;
  ind[1] = 1;
  ind[2] = 2;
  ind[3] = 3;

  scene.loadVertexShader(idVS, "shaders/video_v.cg");
  scene.loadPixelShader(idPS, "shaders/video_p.cg");
  scene.loadPixelShader(idPS_R, "shaders/video_rect_p.cg");

  /* Create primitive and link it with vb/ib/texture */
  scene.addPrimitive(idPrim,"OpenGLWrapper");
  scene.addParamID(idPrim, ChunkPrimParam::VSHADER,"",idVS);
  scene.addParamID(idPrim, ChunkPrimParam::PSHADER,"",idPS);
  scene.addParamID(idPrim, ChunkPrimParam::VBUFFER_ID,"position",idVB);
  scene.addParamID(idPrim, ChunkPrimParam::VBUFFER_ID,"texcoord0",idVB);
  scene.addParamID(idPrim, ChunkPrimParam::VBUFFER_NUMDATA,"texcoord0",1);
  scene.addParamID(idPrim, ChunkPrimParam::IBUFFER_ID,"",idIB);
  scene.addParamEnum(idPrim, ChunkPrimParam::PARAMVSHADER, "ModelViewProj", ChunkPrimParam::ModelViewProjection);
  scene.addParamID(idPrim, ChunkPrimParam::TEXTURE, "texture", idT);

  scene.put(flowvr_scene_port);

  // Get function symbols
#if defined(__APPLE__)
  if ( ! (handle =  dlopen("/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib",RTLD_NOW)) );
#else
  char *gllib = getenv("FLOWVR_OPENGL");
  if (gllib == NULL)
    gllib = strdup("/usr/lib/libGL.so.1");
  
  std::cerr << "GL> Using [" << gllib << "] OpenGL library\n";

  if ( ! (handle =  dlopen(gllib,RTLD_NOW)) )
    {
      std::cerr << "GL> Error opening libGL\n";
      exit(0);
    }
#endif
        
  if ( ! (result = dlsym(handle,"glXSwapBuffers")) )
    {
      std::cerr << "GL> Error getting symbol glXSwapBuffers\n";
      exit(0);
    }
  else
    funcswap     = (glXSwapBuffersFunc_t)result;

  if ( ! (result = dlsym(handle,"glViewport")) )
    {
      std::cerr << "GL> Error getting symbol glViewport\n";
      exit(0);
    }
  else
    funcviewport = (glViewporFunc_t)result;
}

void glXSwapBuffers(Display *dpy, GLXDrawable drawable)
{
  static bool was_rect = false;
  bool scratch = true;
  static int prev_width = -1, prev_height = -1;

#ifdef DEBUG
  std::cerr << __FUNCTION__ << std::endl;
#endif

  if (!flowvr_module->wait())
    exit(0);
#ifdef DEBUG
  std::cerr << "GL> Reading pixels " << winWidth << "x" << winHeight <<" buffer\n";
#endif
  
  ChunkTexture* texture = scene.addTexture(idT, ChunkTexture::RGB,
					   Type::Vec3b,
					   winWidth, winHeight);
  texture->level = 0; // no mipmap
  texture->gen = gen++;
  glReadPixels(0, 0, winWidth, winHeight, GL_RGB, GL_UNSIGNED_BYTE, texture->data());


  // Check if power of two and square
  
  bool rect (winWidth != winHeight || !isPow2(winWidth));
  
  if (rect) {
    if (!was_rect) {
      scene.addParamID(idPrim, ChunkPrimParam::PSHADER,"",idPS_R);
      scratch = false;
    }

    if (prev_width != winWidth || prev_height != winHeight) {
      scene.addParam(idPrim, ChunkPrimParam::PARAMPSHADER,"texsize",Vec2f(winWidth, winHeight));
      scratch = false;
    }
  }
  else {
    if (was_rect) {
      scene.addParamID(idPrim, ChunkPrimParam::PSHADER,"",idPS);
      scratch =false;
    }
  }

  was_rect = rect;
  prev_width = winWidth;
  prev_height = winHeight;

  scene.put(flowvr_scene_port, scratch);	
  
  // Real call to glXSwapBuffers
  if (funcswap)
    (*funcswap)(dpy,drawable);  
}



void glViewport (GLint x, GLint y, GLsizei width, GLsizei height)
{
  if (first) {
    InitTrap();
    first = false;
  }
  if (winWidth == width && winHeight == height)
    return;

#ifdef DEBUG
  std::cerr <<  "glViewport " << x << ", " << y << ", " << width << ", " << height;
#endif
  winWidth = width;
  winHeight = height;

  // Real call to glViewport
  if (funcviewport)
    (*funcviewport)(x,y,width,height);
}

