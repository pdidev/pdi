/******* COPYRIGHT ************************************************
 *                                                                 *
 *                             FlowVR                              *
 *                     Daemon and Base Plugins                     *
 *                                                                 *
 *-----------------------------------------------------------------*
 * COPYRIGHT (C) 2003-2011                by                       *
 * INRIA and                                                       *
 * Laboratoire d'Informatique Fondamentale d'Orleans               *
 * (FRE 2490) ALL RIGHTS RESERVED.                                 *
 *                                                                 *
 * This source is covered by the GNU GPL, please refer to the      *
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
 *    Bruno Raffin,                                                *
 *    Sophie Robert,                                               *
 *    Emmanuel Melin.                                              *
 *                                                                 *
 *******************************************************************
 *                                                                 *
 * File: include/telnet.h                                          *
 *                                                                 *
 * Contacts:                                                       *
 *  Ingo Assenmacher (ingo.assenmacher@imag.fr)                    *
 *                                                                 *
 ******************************************************************/
#ifndef TOPO_H_
#define TOPO_H_

#include <hwloc.h>
#include <deque>

namespace flowvr
{
	namespace ipc
	{
		class MTAtomicInt;
	}
	/**
	 * helper class to abbreviate use of hwloc  and some common
	 * strategies for simple CPU mask creation and stuff.
	 */
	class Topo
	{
	public:
		Topo(bool bInit = true);
		Topo( const Topo &other );

		~Topo(); //<** not meant to be sub-classed */

		void operator=(const Topo &oOther);

		hwloc_cpuset_t getSystemCPUMask() const;
		int           getNumberOfPhysicalCPUs() const;
		int           getNumberOfLogicalCPUs() const;
		int           getNumberOfCores() const;

		/**
		 * NUMA mem in bytes
		 */
		unsigned long int getL3CacheSize(int nPhysicalCPU) const;
		unsigned long int getL2CacheSize(int OSCoreId) const;
		unsigned long int getL1CacheSize(int OSCoreId) const;

		std::deque<hwloc_cpuset_t> getL2GroupSet() const;
		std::deque<hwloc_cpuset_t> getL3GroupSet() const;
		std::deque<hwloc_cpuset_t> getSocketGroupSet() const;

		bool getL2CacheShared(int OSCoreId) const;
		bool getL1CacheShared(int OSCoreId) const;

		/**
		 * for advanced use: read only structure
		 */
		struct _topology
		{
			hwloc_topology_t    t_topology;
                        hwloc_obj_type_t    t_topology_info;
		} topology;


		bool getIsValid() const;
	private:
		int getLevelCount(int depth) const;

		bool valid;
		flowvr::ipc::MTAtomicInt *refCnt;
	};
}

#endif /* TOPO_H_ */
