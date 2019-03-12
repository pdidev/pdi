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
* File: ./src/viewers/terrain.cpp                                 *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#include <flowvr/module.h>
#include <flowvr/render/chunk.h>
#include <flowvr/render/chunkwriter.h>

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

using namespace flowvr::render;

#define ERRMSG(msg) (std::cerr << __FILE__ << ":" << __LINE__ << "(" << __PRETTY_FUNCTION__ <<"): " << msg << std::endl, false)

#define CHID(a,b,c,d) ( (((unsigned)(a))) | (((unsigned)(b))<<8) | (((unsigned)(c))<<16) | (((unsigned)(d))<<24) )

inline float lerp(float a,float b,float f)
{
  return a+(b-a)*f;
}
int cx = 181; int cy = 240;

class TerragenTerrain
{
public:
  short nx, ny;
  Vec3f scale;
  float hbase;
  float hscale;
  short* height;

  TerragenTerrain()
  : nx(0), ny(0), scale(30,30,30), hbase(0), hscale(1), height(NULL) {}

  bool load(std::string filename)
  {
    std::cout << "Loading terrain "<<filename<<std::endl;

    if (height != NULL)
    {
      delete[] height;
      height = NULL;
    }

#if defined(FLOWVR_RENDER_PREFIX)
# define FLOWVR_STR(f) #f
# define FLOWVR_STR2(f) FLOWVR_STR(f)
    std::string store;
    flowvr::utils::FilePath path("share/flowvr-render/data", "FLOWVR_DATA_PATH", store, "FLOWVR_RENDER_PREFIX", FLOWVR_STR2(FLOWVR_RENDER_PREFIX));
    std::cout << "FilePath resolved: [ " << store << "] for FLOWVR_DATA_PATH" << std::endl;
# undef FLOWVR_STR2
# undef FLOWVR_STR
#else
# warning No default search path for flowvr-render data. Relying only on FLOWVR_DATA_PATH
    flowvr::utils::FilePath path;
#endif

    path.findFile(filename);

    FILE* f = fopen(filename.c_str(),"rb");
    if (!f) return ERRMSG("File not found");

    char head[16];

    fread(head,16,1,f);

    if (memcmp(head,"TERRAGENTERRAIN ",sizeof(head)))
    {
      fclose(f);
      return ERRMSG("Bad Header");
    }

    bool stop = false;

    while (!stop && !feof(f))
    {
      int chunk=0;
      fread(&chunk,sizeof(int),1,f);
      switch (chunk)
      {
      case CHID('S','I','Z','E'):
      {
	short s;
	fread(&s,sizeof(s),1,f);
	nx = ny = s+1;
	fread(&s,sizeof(s),1,f);
	break;
      }
      case CHID('X','P','T','S'):
      {
	short s;
	fread(&s,sizeof(s),1,f);
	nx = s;
	fread(&s,sizeof(s),1,f);
	break;
      }
      case CHID('Y','P','T','S'):
      {
	short s;
	fread(&s,sizeof(s),1,f);
	ny = s;
	fread(&s,sizeof(s),1,f);
	break;
      }
      case CHID('S','C','A','L'):
      {
	fread(scale.ptr(),sizeof(float),3,f);
	break;
      }
      case CHID('A','L','T','W'):
      {
	short s;
	fread(&s,sizeof(short),1,f); hbase = s;
	fread(&s,sizeof(short),1,f); hscale = s/65536.0f;
	height = new short[nx*ny];
	fread(height,sizeof(short),nx*ny,f);
	break;
      }
      case CHID('E','O','F',' '):
      {
	stop = true;
	break;
      }
      }
    }
    fclose(f);
    if (height == NULL) return ERRMSG("No terrain found.");
    std::cout << "Terrain Loaded: "<<nx<<"x"<<ny<<std::endl;
    return true;
  }

  short getHeightI(int x,int y)
  {
    return height[y*nx+x];
  }

  float getHeight(float x, float y)
  {
    float fx = cx+(x/scale.x());
    float fy = cy+(y/scale.y());
    int x0 = int(fx+100000)-100000;
    int y0 = int(fy+100000)-100000;
    if ((unsigned)x0>=(unsigned)(nx-1) || (unsigned)y0>=(unsigned)(ny-1)) return 0;
    int i0 = y0*nx+x0;
    fx -= x0;
    fy -= y0;
    float h = lerp(lerp(height[i0],height[i0+1],fx),lerp(height[i0+nx],height[i0+nx+1],fx),fy);
    return (hbase + hscale*h)*scale.z();
  }

};

float get(flowvr::InputPort& p, float& val)
{
  flowvr::Message m;
  p.getModule()->get(&p,m);
  if (m.data.getSize()<(int)sizeof(float)) return 0;
  float last = val;
  val = *m.data.getRead<float>();
  return val-last;
}

float getSum(flowvr::InputPort& p, float& val, float speed=1)
{
  flowvr::Message m;
  p.getModule()->get(&p,m);
  if (m.data.getSize()<(int)sizeof(float)) return 0;
  float depl = *m.data.getRead<float>() * speed;
  val+=depl;
  return depl;
}

int main(int argc, char** argv)
{

  flowvr::InputPort pRotX("rotX");
  flowvr::InputPort pRotY("rotY");
  flowvr::InputPort pTransX("transX");
  flowvr::InputPort pTransY("transY");
  flowvr::InputPort pZoom("zoom");
  SceneOutputPort pOut("scene");
  flowvr::OutputPort pOutMat("matrix");
  flowvr::OutputPort pTerrain("terrain");
  flowvr::OutputPort pOutFluid("fluidpos");
  flowvr::StampInfo  terrainN("N",flowvr::TypeArray::create(2,flowvr::TypeInt::create()));
  flowvr::StampInfo  terrainP("P",flowvr::TypeArray::create(2,flowvr::TypeFloat::create()));
  flowvr::StampInfo  terrainS("S",flowvr::TypeArray::create(2,flowvr::TypeFloat::create()));
  flowvr::StampInfo  terrainH("H",flowvr::TypeArray::create(2,flowvr::TypeFloat::create()));
  pTerrain.stamps->add(&terrainN);
  pTerrain.stamps->add(&terrainP);
  pTerrain.stamps->add(&terrainS);
  pTerrain.stamps->add(&terrainH);

  std::vector<flowvr::Port*> ports;
  ports.push_back(&pRotX);
  ports.push_back(&pRotY);
  ports.push_back(&pTransX);
  ports.push_back(&pTransY);
  ports.push_back(&pZoom);
  ports.push_back(&pOut);
  ports.push_back(&pOutMat);
  ports.push_back(&pOutFluid);
  ports.push_back(&pTerrain);

  flowvr::ModuleAPI* module = flowvr::initModule(ports);  
  if (module == NULL)
  {
    return 1;
  }

  flowvr::render::ChunkRenderWriter scene;

  ID idP = 0x300; //module->generateID();
  ID idT = 0x301; //module->generateID();
  ID idVB = 0x302; //module->generateID();
  ID idIB = 0x303; //module->generateID();
  ID idVS = 0x304; //module->generateID();
  ID idPS = 0x305; //module->generateID();

  std::cout << "idVS="<<idVS<<std::endl;
  std::cout << "idPS="<<idPS<<std::endl;

  std::string ftexture = "images/valley2.jpg";
  std::string fheight = "valley2.ter";
  if (argc>=2) ftexture = argv[1];
  if (argc>=3) fheight  = argv[2];
  int nx = 257; int ny = 257;

  TerragenTerrain terrain;
  if (!terrain.load(fheight))
  {
    module->close();
    return 2;
  }

  Vec3f position0;
  Vec3f position(-25,0,0);
  Vec3f rotation;
  float zoom = 1;

  if (cx >= terrain.nx) cx = terrain.nx/2;
  if (cy >= terrain.ny) cy = terrain.ny/2;
  if (cx-nx/2<0) nx = cx*2;
  if (cy-ny/2<0) ny = cy*2;
  if (cx-nx/2+nx>terrain.nx) nx = (terrain.nx-cx)*2-1;
  if (cy-ny/2+ny>terrain.ny) ny = (terrain.ny-cy)*2-1;

  terrain.scale = Vec3f(1.0f,1.0f,2.0f);
  //terrain.hbase -= 18.033f;
  terrain.hbase = -terrain.hscale*terrain.height[(cy)*terrain.nx+(cx-25)];

  std::cout << "Using data centered @ "<<cx<<"x"<<cy<<" size "<<nx<<"x"<<ny<<" scale "<<terrain.scale.x()<<"x"<<terrain.scale.y()<<"x"<<terrain.scale.z()<<" z="<<terrain.hbase<<"+h*"<<terrain.hscale<<std::endl;

  if (!scene.loadTexture(idT,ftexture))
    scene.addDefaultTexture(idT);

  int dataType[1] = { Type::Vec3s };

  short hmin = 0xffff;
  short hmax = 0;

  int xmin=0,xmax=1,ymin=0,ymax=1; // fluid interval

  {
    short* height = terrain.height+(cy-ny/2)*terrain.nx+(cx-nx/2);
    for (int y=0;y<ny;y++)
    {
      for (int x=0;x<nx;x++)
      {
	if (height[x] < hmin)
	{
	  hmin = height[x];
	  xmin=xmax=x;
	  ymin=ymax=y;
	}
	else if (height[x] == hmin)
	{
	  if (x<xmin) xmin=x; else if (x>xmax) xmax=x;
	  if (y<ymin) ymin=y; else if (y>ymax) ymax=y;
	}
	if (height[x] > hmax) hmax = height[x];
      }
      height+=terrain.nx;
    }
  }

  std::cout << "height min="<<hmin<<" max="<<hmax<<std::endl;

  ChunkVertexBuffer* vb = scene.addVertexBuffer(idVB, nx*ny, 1, dataType, BBox(Vec3f(0,0,hmin),Vec3f(nx-1,ny-1,hmax)));
  {
    Vec<3,short>* vertex = (Vec<3,short>*)vb->data();
    short* height = terrain.height+(cy-ny/2)*terrain.nx+(cx-nx/2);
    for (int y=0;y<ny;y++)
    {
      for (int x=0;x<nx;x++)
      {
	vertex->x() = x;
	vertex->y() = y;
	vertex->z() = height[x];
	++vertex;
      }
      height+=terrain.nx;
    }
  }

/*
  std::cout << "Primitive mode: QUAD\n";
  ChunkIndexBuffer* ib = scene.addIndexBuffer(idIB, 4*(nx-1)*(ny-1), Type::Int, ChunkIndexBuffer::Quad);
  {
    unsigned int* ind = (unsigned int*)ib->data();
    for (int y=0;y<ny-1;y++)
    {
      for (int x=0;x<nx-1;x++)
      {
	ind[0] = (y  )*nx+(x  );
	ind[1] = (y  )*nx+(x+1);
	ind[2] = (y+1)*nx+(x+1);
	ind[3] = (y+1)*nx+(x  );
	ind+=4;
      }
    }
  }
*/

/*
  std::cout << "Primitive mode: TRIANGLE STRIP\n";
  int restart = -1;
  ChunkIndexBuffer* ib = scene.addIndexBuffer(idIB, 2*(nx)*(ny-1)+(ny-2), Type::Int, ChunkIndexBuffer::TriangleStrip);
  ib->restart = restart;
  {
    unsigned int* ind = (unsigned int*)ib->data();
    for (int y=0;y<ny-1;y++)
    {
      if (y)
	*(ind++) = (unsigned int)restart; // start a new strip
      for (int x=0;x<nx;x++)
      {
	ind[0] = (y  )*nx+(x  );
	ind[1] = (y+1)*nx+(x  );
	ind+=2;
      }
    }
  }
*/

  std::cout << "Primitive mode: TRIANGLE FAN\n";
  int restart = -1;
  int nindex = 
      ((nx-1)/2)*((ny-1)/2)*(10+1) // full circles
      +((nx&1)?0:((ny-1)/2)*(6+1)) // half circles if nx is even
      +((ny&1)?0:((nx-1)/2)*(6+1)) // half circles if ny is even
      +((nx&1 || ny&1)?0:   (4+1)) // final quad if both nx and ny are even
      -1; // last fan does not need a restart index
  ChunkIndexBuffer* ib = scene.addIndexBuffer(idIB, nindex, Type::Int, ChunkIndexBuffer::TriangleFan);
  ib->restart = restart;
  {
    unsigned int* ind = (unsigned int*)ib->data();
    unsigned int* start=ind;
    for (int y=1;y<ny;y+=2)
    {
      for (int x=1;x<nx;x+=2)
      {
	if (ind!=start)
	    *(ind++) = (unsigned int)restart; // start a new fan
	*(ind++) = (y  )*nx+(x  );
	if (y<ny-1)
	*(ind++) = (y+1)*nx+(x  );
	if (y<ny-1)
	*(ind++) = (y+1)*nx+(x-1);
	*(ind++) = (y  )*nx+(x-1);
	*(ind++) = (y-1)*nx+(x-1);
	*(ind++) = (y-1)*nx+(x  );
	if (x<nx-1)
	*(ind++) = (y-1)*nx+(x+1);
	if (x<nx-1)
	*(ind++) = (y  )*nx+(x+1);
	if (x<nx-1 && y<ny-1)
	*(ind++) = (y+1)*nx+(x+1);
	if (x<nx-1 && y<ny-1)
	*(ind++) = (y+1)*nx+(x  );
      }
    }
  }

  scene.loadVertexShader(idVS, "shaders/terrain_v.cg");
  scene.loadPixelShader(idPS, "shaders/terrain_p.cg");

  scene.addPrimitive(idP,"Terrain");
  //scene.addParam(idP, ChunkPrimParam::ORDER,"",0);
  scene.addParamID(idP, ChunkPrimParam::VSHADER,"",idVS);
  scene.addParamID(idP, ChunkPrimParam::PSHADER,"",idPS);
  scene.addParamID(idP, ChunkPrimParam::VBUFFER_ID,"position",idVB);
  scene.addParamID(idP, ChunkPrimParam::IBUFFER_ID,"",idIB);
  scene.addParamEnum(idP, ChunkPrimParam::PARAMVSHADER, "ModelViewProj", ChunkPrimParam::ModelViewProjection);
  scene.addParam(idP, ChunkPrimParam::PARAMVSHADER, "TextureScale", Vec2f(1.0f/nx,-1.0f/ny));
  scene.addParamID(idP, ChunkPrimParam::TEXTURE, "texture", idT);
  scene.addParam(idP,ChunkPrimParam::TRANSFORM_SCALE,"",Vec3f(terrain.scale.x(),terrain.scale.y(),terrain.hscale*terrain.scale.z()));
  scene.addParam(idP,ChunkPrimParam::TRANSFORM_POSITION,"",Vec3f(-nx/2*terrain.scale.x(),-ny/2*terrain.scale.y(),terrain.hbase*terrain.scale.z()));

  scene.put(&pOut);

  flowvr::MessageWrite mt;
  mt.data = module->alloc(nx*ny*sizeof(short));
  mt.stamps.write(terrainN[0],nx);
  mt.stamps.write(terrainN[1],ny);
  mt.stamps.write(terrainP[0],-nx/2*terrain.scale.x());
  mt.stamps.write(terrainP[1],-ny/2*terrain.scale.y());
  mt.stamps.write(terrainS[0],terrain.scale.x());
  mt.stamps.write(terrainS[1],terrain.scale.y());
  mt.stamps.write(terrainH[0],terrain.scale.z()*terrain.hbase);
  mt.stamps.write(terrainH[1],terrain.scale.z()*terrain.hscale);
  {
    short* height = terrain.height+(cy-ny/2)*terrain.nx+(cx-nx/2);
    for (int y=0;y<ny;y++)
    {
      memcpy(mt.data.getWrite<short>(y*nx*sizeof(short)), height, nx*sizeof(short));
      height+=terrain.nx;
    }
  }

  module->put(&pTerrain, mt);

  // Fluid position
  {

    short fluidh = hmin+(short)(0.3f/terrain.hscale);

    short* height = terrain.height+(cy-ny/2)*terrain.nx+(cx-nx/2);
    for (int y=0;y<ny;y++)
    {
      for (int x=0;x<nx;x++)
      {
	if (height[x] <= fluidh)
	{
	  if (x<xmin) xmin=x; else if (x>xmax) xmax=x;
	  if (y<ymin) ymin=y; else if (y>ymax) ymax=y;
	}
      }
      height+=terrain.nx;
    }

    std::cout << "Fluid pos: <"<<xmin<<','<<ymin<<">-<"<<xmax<<','<<ymax<<") h="<<fluidh<<std::endl;
    flowvr::MessageWrite m;
    m.data = module->alloc(sizeof(Mat4x4f));
    Mat4x4f& fluid = *m.data.getWrite<Mat4x4f>();
    fluid.identity();
    // Scale
    fluid[0][0] = (xmax-xmin+1)*terrain.scale.x();
    fluid[1][1] = (ymax-ymin+1)*terrain.scale.y();
    fluid[2][2] = terrain.scale.z();
    // Position
    fluid[0][3] = (xmin-nx/2-0.5f)*terrain.scale.x();
    fluid[1][3] = (ymin-ny/2-0.5f)*terrain.scale.y();
    fluid[2][3] = (terrain.hbase+fluidh*terrain.hscale)*terrain.scale.z();
    module->put(&pOutFluid,m);
  }

  module->wait();

  flowvr::BufferPool pool;

  if (pRotX.isConnected())
  {
    float rotSpeed = 0.01f;
    float speed = 0.04f;
    do
    {
      Vec3f depl;
      depl.x() = getSum(pTransX,position0.x(),speed);
      depl.y() = -getSum(pTransY,position0.y(),speed);
      getSum(pRotX,rotation.x(),rotSpeed);
      getSum(pRotY,rotation.y(),rotSpeed);
      get(pZoom,zoom);

      Mat3x3f mrot;
      Quat qx,qy,qz;
      qx.fromAngAxis(M_PI/2*rotation.x(),Vec3f(0,0,1));
      qy.fromAngAxis(M_PI/2*rotation.y(),Vec3f(1,0,0));
      qz.fromAngAxis(M_PI/2*rotation.z(),Vec3f(0,1,0));
      Quat q = qz*qy*qx;

      q.toMatrix(&mrot);

      Vec3f newPos = position+mrot*depl;
      newPos.z() = terrain.getHeight(newPos.x(), newPos.y()); //+(zoom+1);

      position = newPos;

      Mat3x3f xrot;
      qx.toMatrix(&xrot);

      Vec3f pcam = position+xrot*(Vec3f(0,-2,0)*(1+zoom))+Vec3f(0,0,1.5);
      float zTer = terrain.getHeight(pcam.x(), pcam.y())+1;
      if (zTer > pcam.z()) pcam.z()=zTer;

      Mat4x4f m;
      m.identity();
      m = mrot;
      m(0,3) = pcam[0];
      m(1,3) = pcam[1];
      m(2,3) = pcam[2];

      Mat4x4f mcam; mcam.identity();

      Mat3x3f rotcam;
      Quat qcam; qcam.fromAngAxis(-M_PI/2,Vec3f(1,0,0));
      qcam.toMatrix(&rotcam);

      mcam = rotcam;
      m = m*mcam;

      scene.addParam(ID_CAMERA,ChunkPrimParam::TRANSFORM,"",m);

      mrot.identity();
      m = mrot;
      m(0,3) = position[0];
      m(1,3) = position[1];
      m(2,3) = position[2];

      flowvr::MessageWrite msg;
      msg.data = pool.alloc(module,sizeof(Mat4x4f));
      *msg.data.getWrite<Mat4x4f>() = m;
      module->put(&pOutMat, msg);

      scene.put(&pOut);

    }
    while (module->wait());
  }

  module->close();

  return 0;
}
