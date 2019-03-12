/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                             Utils                               *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490) ALL RIGHTS RESERVED.                                 *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Ronan Gaugne,                                                *
*    Valerie Gouranton,                                           *
*    Loick Lecointre,                                             *
*    Sebastien Limet,                                             *
*    Clement Menier,                                              *
*    Bruno Raffin,                                                *
*    Sophie Robert,                                               *
*    Emmanuel Melin.                                              *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: include/flowvr/gltrace/object.h                           *
*                                                                 *
* Contacts:                                                       *
*  08/2004 Julien Garand                                          *
*                                                                 *
******************************************************************/

class Object
{
public:
  std::string name;
  std::string hostname;
  Host* host;

  int pos;
  bool display;

  std::vector<Display*> drawfunc;


  void draw()
  {
    if(!display) return;

    for(unsigned int i=0;i<drawfunc.size();i++)
      drawfunc[i]->draw(pos);
  };

  flowvr::xml::DOMElement xmlDesc()
  {
    flowvr::xml::DOMElement res=flowvr::xml::DOMElement("object");

    res.SetAttribute("id",name);
    res.SetAttribute("host",hostname);

    flowvr::xml::DOMElement disp = flowvr::xml::DOMElement("objdisplay");

    if(display)
    {
      disp.SetAttribute("active","YES");
      disp.SetAttribute("pos",pos);

      for(unsigned int i=0; i<drawfunc.size(); i++)
        disp.InsertEndChild(drawfunc[i]->xmlDesc());
    }
    else
      disp.SetAttribute("active","NO");

    res.InsertEndChild(disp);

    return res;
  };


  Object()
  {
    display = false;
  };

  ~Object()
  {
    for(unsigned int i=0; i<drawfunc.size(); i++)
      delete drawfunc[i];

    drawfunc.clear();
  };
};
