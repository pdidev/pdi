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
* File: include/flowvr/gltrace/host.h                             *
*                                                                 *
* Contacts:                                                       *
*  08/2004 Julien Garand                                          *
*                                                                 *
******************************************************************/

class Host
{
public:
  std::string name;
  std::vector<std::string> loggerlist;

  virtual double cycleToTime(cycle_t cycle)=0;

  virtual void addPing(int ID, PingInfo newping)=0;

  virtual void calculateCorrespondance()=0;

  virtual flowvr::xml::DOMElement xmlDesc();

  Host(std::string newname);

  virtual ~Host();
};

class Host_Fastest: public Host
{
protected:
  PingInfo P1;
  PingInfo P2;

  int p1;  // ping id of P1
  int p2;  // ping id of P2

  double coeffA;
  double coeffB;

public:
  virtual void addPing(int ID, PingInfo newping);

  virtual double cycleToTime(cycle_t cycle);

  virtual void calculateCorrespondance();

  virtual flowvr::xml::DOMElement xmlDesc();

private:
  double getPrecision();

public:
  Host_Fastest(std::string newname);

  virtual ~Host_Fastest();
};


class Host_Average: public Host
{
protected:
  std::vector<PingInfo> P1;
  std::vector<PingInfo> P2;

  int p1;  // ping id of P1
  int p2;  // ping id of P2

  double coeffA;
  double coeffB;

public:
  virtual void addPing(int ID, PingInfo newping);

  virtual double cycleToTime(cycle_t cycle);

  virtual void calculateCorrespondance();

  virtual flowvr::xml::DOMElement xmlDesc();

private:
  double getPrecision();

public:
  Host_Average(std::string newname);

  virtual ~Host_Average();
};
