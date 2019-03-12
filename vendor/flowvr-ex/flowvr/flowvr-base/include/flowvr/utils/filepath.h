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
* File: ./include/flowvr/utils/filepath.h                         *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_UTILS_FILEPATH_H
#define FLOWVR_UTILS_FILEPATH_H

#include <vector>
#include <string>

namespace flowvr
{

namespace utils
{

/// Helper class to find file in a list of directories.
///
/// Each file is searched as follow:
///
/// 1: Using the specified filename in current directory.
/// 2: In the directory path specified using addPath method.
/// 3: In the directory path specified using an environment variable (default to FLOWVR_DATA_PATH).
///
/// For file name starting with '/', './' or '../' only the first step is used.
///
/// A path is considered as a concatenation of directories separated by :
class FilePath
{
public:
  /// Initialize the set of paths from an environment variable.
  FilePath(const char* envVar = "FLOWVR_DATA_PATH");
  
  FilePath(const char * pathsuffix,
		   const char* envVar,
		   std::string &strStore,
		   const char * prefixVariable = NULL,
		   const char * prefixPath = NULL);

  /// Adds a path to the set of paths.
  void addPath(const std::string& path);
  std::vector<std::string> getAllPaths() const;

  /// Search file in a given path.
  static bool findFileIn(std::string& filename, const std::string& path);

  /// Search file in a given path and give the repertoire where the file is located
  static bool findFileInRep(const std::string& filename, const std::string& path, std::string& rep);

  /// Find file using the stored set of paths.
  bool findFile(std::string& filename);

  /// Give the repertoire where the file is located
  bool findFileRep(const std::string& filename, std::string& repertory);

  /// Print the list of path to std::cout
  void print();

  /**
   * @brief single, simple test to see whether a given path really points to an entry in the directory.
   *
   * @param path absolute or relative path to directory or file
   * @return true if there is an entry in the directory tree that matches the path given
   */
  bool exists( const std::string &path ) const;

private:
  /// Vector of paths.
  std::vector<std::string> vpath;

};

} // namespace utils

} // namespace flowvr

#endif

