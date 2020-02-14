/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                       Template Library                          *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA.  ALL RIGHTS RESERVED.                                    *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
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
* File: ./include/flowvr/chunkevents.h                            *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_CHUNKEVENTS_H
#define FLOWVR_CHUNKEVENTS_H

#include "chunk.h"
#include "chunkwriter.h"
#include "chunkreader.h"
#include "mat.h"

#include <flowvr/moduleapi.h>

namespace ftl
{

// Based in the GLUT API macro definitions -- the special key codes:
#define  FLOWVR_KEY_F1                        0x01
#define  FLOWVR_KEY_F2                        0x02
#define  FLOWVR_KEY_F3                        0x03
#define  FLOWVR_KEY_F4                        0x04
#define  FLOWVR_KEY_F5                        0x05
#define  FLOWVR_KEY_F6                        0x06
#define  FLOWVR_KEY_F7                        0x07
#define  FLOWVR_KEY_F8                        0x08
#define  FLOWVR_KEY_F9                        0x09
#define  FLOWVR_KEY_F10                       0x0A
#define  FLOWVR_KEY_F11                       0x0B
#define  FLOWVR_KEY_F12                       0x0C
#define  FLOWVR_KEY_LEFT                      0x64
#define  FLOWVR_KEY_UP                        0x65
#define  FLOWVR_KEY_RIGHT                     0x66
#define  FLOWVR_KEY_DOWN                      0x67
#define  FLOWVR_KEY_PAGE_UP                   0x68
#define  FLOWVR_KEY_PAGE_DOWN                 0x69
#define  FLOWVR_KEY_HOME                      0x6A
#define  FLOWVR_KEY_END                       0x6B
#define  FLOWVR_KEY_INSERT                    0x6C

/// This assure that the messages have the short size as possible and no alignment
#if defined _MSC_VER
# pragma pack(push)
# pragma pack (1)
#else
# pragma pack(push,1)
#endif

/// Base class for IO communication events
struct ChunkEvent : public Chunk
{
public:

  enum EventTypes {
    BUTTON	= 0x01,
    SLIDER	= 0x02,
    KEYBOARD	= 0x03,
    MOUSE	= 0x04,
    STRING	= 0x05,
    POSITION	= 0x06,
  };

  ID id;	// This can be used to identify the device

};



/// Helper class to construct messages containing chunk events for buttons
/// It supports digital values (0/1) pressed/released
struct ChunkEventButton : public ChunkEvent
{
public:

  bool val;		// (0/1) pressed/released
  unsigned char key;	// key name

  static const int TYPE = ChunkEvent::BUTTON | Chunk::EVENT;

};


/// Helper class to construct messages containing chunk events for sliders.
/// It supports analog values (0 to 1)
struct ChunkEventSlider : public ChunkEvent
{
public:

  float val;		// key value
  unsigned char key;	// key name

  static const int TYPE=ChunkEvent::SLIDER | Chunk::EVENT;

};


/// Helper class to construct messages containing chunk events for keyboards.
/// It is more powerful than simple keys but it has a bigger overhead
struct ChunkEventKeyboard : public ChunkEvent
{
public:

  unsigned int key;		// Stores the button
  bool val;			// (0/1) pressed/released
  bool special;			// in the case of special keys like F1, CTRL, SHIFT, etc...
  int modifier;			// if the key is being pressed with a special key at the same time

  static const int TYPE=ChunkEvent::KEYBOARD | Chunk::EVENT;

  enum
  {
	  MOD_NONE = 0,
	  MOD_SHIFT_DOWN=1,
	  MOD_CTRL_DOWN =2,
	  MOD_ALT_DOWN  =4
  };

};


/// Helper class to construct messages containing chunk events for mouses.
/// It supports analog values (0 to 1)
struct ChunkEventMouse : public ChunkEvent
{
public:

  unsigned char mouseKeys;	// Stores each button as a bit, "1" means pressed
				// The buttons are stored from left to right

  float mouseTranslation[2];
  int   mouseWheelState,
        mouseWheelDir;
  int   released; // (0/1/2) none/pressed/released
  int   modifier; // direct copy of GLUT constant

  static const int TYPE=ChunkEvent::MOUSE | Chunk::EVENT;

  enum
  {
	  MOD_NONE = 0,
	  MOD_SHIFT_DOWN=1,
	  MOD_CTRL_DOWN =2,
	  MOD_ALT_DOWN  =4
  };

};


/// Helper class to construct messages containing chunk events for strings.
/// It supports any string and can be used for commands like speech recognition.
/// String needs to be passed using additional space allocated for the chunk:
/// e.g. evtString = chunkWriter.addChunk<ChunkEventString>(str.length());
struct ChunkEventString : public ChunkEvent
{
public:
  static const int TYPE=ChunkEvent::STRING | Chunk::EVENT;
  const void* data() const
  {
    return (const void*)(this+1);
  }

  void* data()
  {
    return (void*)(this+1);
  }

  int dataSize() const
  {
    return size+sizeof(Chunk)-((const char*)(data())-(const char*)(this));
  }
};


/// Helper class to construct messages containing chunk events for positioning devices.
/// It supports a 4 X 4 matrix for transformation
struct ChunkEventPosition : public ChunkEvent
{
public:

  bool AbsolutRelative;
  ftl::Mat4x4f Transform;

  static const int TYPE=ChunkEvent::POSITION | Chunk::EVENT;

};

#pragma pack(pop)


/// Generic class used to write the chunks for IO events
class ChunkEventWriter : public ChunkWriter
{
public:

  ChunkEventWriter() : ChunkWriter()
  {
  }

  virtual ~ChunkEventWriter() {}

  /// Insert an button event, informing which specific key and its value
  ChunkEventButton *addEventButton(unsigned char key, bool val, flowvr::ID id = 0)
  {

    // This routine use the addChunk template to include a chunk event
    // The addChunk just uses pointer the data will be copied after
    ChunkEventButton* ev = addChunk<ChunkEventButton>( sizeof(unsigned char) + sizeof(bool) );

    ev->id = id;			// Copying the id to the chunk

    ev->val = val;			// Copying the values to the chunk
    ev->key = key;			// Copying the key to the chunk

    return ev;				// Returning the chunk pointer

  }

  // Insert a slider event, informing which specific key and its value
  ChunkEventSlider *addEventSlider(unsigned char key, float val, flowvr::ID id = 0)
  {

    ChunkEventSlider* ev = addChunk<ChunkEventSlider>( sizeof(unsigned char) + sizeof(float) );

    ev->id = id;			// Copying the id to the chunk

    ev->val = val;			// Copying the values to the chunk
    ev->key = key;			// Copying the key to the chunk

    return ev;				// Returning the chunk pointer

  }

  // Insert an keyboard event, informing which specific key and its values
  ChunkEventKeyboard *addEventKeyboard(unsigned int key, bool val, bool special, int modifier, flowvr::ID id = 0)
  {

    // This routine use the addChunk template to include a chunk event
    // The addChunk just uses pointer the data will be copied after
    ChunkEventKeyboard* ev = addChunk<ChunkEventKeyboard>( sizeof(unsigned int) + (2 * sizeof(bool)) + sizeof(int) );

    ev->id = id;			// Copying the id to the chunk

    ev->key = key;			// Copying the key to the chunk
    ev->val = val;			// Copying the pressing value to the chunk
    ev->special = special;		// Copying the special tag to the chunk
    ev->modifier = modifier;		// Copying the modifier value to the chunk

    return ev;				// Returning the chunk pointer

  }


  // Insert a mouse event, informing the keys pressed and positions
  ChunkEventMouse *addEventMouse(unsigned char keys, float *trans,
		                         flowvr::ID id = 0,
		                         int mouseWheelState = 0,
		                         int mouseWheelDir = 0,
		                         int btState = 0,
		                         int modifier = 0)
  {

    ChunkEventMouse* ev = addChunk<ChunkEventMouse>( sizeof(unsigned char) // key
    		                                      + (2 * sizeof(float)) // translation
    		                                      + (4 * sizeof( int ) ) // mouse wheel+btnstate+modifer
													);

    ev->id = id;			// Copying the id to the chunk

    ev->mouseTranslation[0] = trans[0];	// Copying the X position to the chunk
    ev->mouseTranslation[1] = trans[1];	// Copying the Y position to the chunk
    ev->mouseKeys = keys;		// Copying the key to the chunk
    ev->mouseWheelState = mouseWheelState;
    ev->mouseWheelDir = mouseWheelDir;
    ev->released      = btState;
    ev->modifier      = modifier;

    return ev;				// Returning the chunk pointer

  }

  // Insert a string event
  ChunkEventString *addEventString(std::string command, flowvr::ID id = 0)
  {

    ChunkEventString* ev = addChunk<ChunkEventString>(command.length()+1);

    ev->id = id;			// Copying the id to the chunk
    strncpy(static_cast<char *>(ev->data()), command.c_str(), command.length()+1);			// Copying the string to the chunk

    return ev;				// Returning the chunk pointer

  }

  /**
   *  Insert a position event, this is effectively a 4x4 matrix, given as sequence of floats
   *  @param absrel the transform is absolute (true) or relative (false)
   */
  ChunkEventPosition *addEventPosition(bool absrel, float *trans, flowvr::ID id = 0)
  {

    ChunkEventPosition* ev = addChunk<ChunkEventPosition>( sizeof(bool) + (16 * sizeof(float)) );

    ev->id = id;			// Copying the id to the chunk

    ev->AbsolutRelative = absrel;
    // copy-in construction (implicit) from array of floats
    // to Mat4x4f
    ev->Transform = trans;


    return ev;				// Returning the chunk pointer

  }


};


/// Generic class used to read the chunks for IO events
class ChunkEventReader : public ChunkReader
{
public:

  bool processChunk(const MsgChunk<ChunkEventButton>& data);
  bool processChunk(const MsgChunk<ChunkEventSlider>& data);
  bool processChunk(const MsgChunk<ChunkEventKeyboard>& data);
  bool processChunk(const MsgChunk<ChunkEventMouse>& data);
  bool processChunk(const MsgChunk<ChunkEventString>& data);
  bool processChunk(const MsgChunk<ChunkEventPosition>& data);

};

} // namespace ftl

#endif
