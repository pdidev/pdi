/******* COPYRIGHT ************************************************
 *                                                                 *
 *                             FlowVR                              *
 *                         Base Libraries                          *
 *                                                                 *
 *-----------------------------------------------------------------*
 * COPYRIGHT (C) 2003-2011                by                       *
 * INRIA and                                                       *
 * Laboratoire d'Informatique Fondamentale d'Orleans               *
 * (FRE 2490). ALL RIGHTS RESERVED.                                *
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
 *    Bruno Raffin,                                                *
 *    Sophie Robert,                                               *
 *    Emmanuel Melin.                                              *
 *                                                                 * 
 *******************************************************************
 *                                                                 *
 * File: include/flowvr/message.h                                  *
 *                                                                 *
 * Contacts:                                                       *
 *  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
 *                                                                 *
 ******************************************************************/
#ifndef FLOWVR_MESSAGE_H
#define FLOWVR_MESSAGE_H

#include "flowvr/buffer.h"
#include "flowvr/stamp.h"
#include <vector>

namespace flowvr
{

  /** 
   * @brief FlowVR message with write access enabled
   *
   * This class is typically used by data sources / modules that send data
   * to other module. It directly allows to modify stamps and data of the
   * message. It is but a helper structure to contain the write-enabled
   * interfaces for data manipulation, not a real message.
   *
   * @ingroup Messagehandling
   */
  class MessageWrite
  {
      public:

		  /// @name state
		  /// @{
          /**
           * @brief  clear the stamp list and the data buffer
           *
           * In case you want to re-use this message write, call clear()
           * and Allocator::alloc() to reserve new memory for data.
           * Stamps will typically resize correctly upon a call to StampsWrite::write().
           */
          void clear()
          {
              stamps.clear();
              data.clear();
          }
          /// @}


          /// @name operators
          /// @{
          /**
           * @brief Message comparison operation
           */
          bool operator==(const MessageWrite& m) const
          {
              return stamps==m.stamps && data==m.data;
          }

          /**
           * @brief Message comparison operation
           */
          bool operator!=(const MessageWrite& m) const
          {
              return !(*this == m);
          }

          /// @}

          StampsWrite stamps; /**< Message stamp list */
          BufferWrite data;   /**< Message data buffer */
  }; 



  /** 
   * @brief FlowVR message with  writable stamps but read-only data buffer.
   *
   * MessagePut is useful for example for filters that want to pass the
   * data but modify the stamps.
   *
   * @ingroup Messagehandling
   */
  class MessagePut
  {
      public:
		  /// @name construction and conversion
	      /// @{
		  /**
		   * @brief create an invalid MessagePut
		   *
		   * Creates a message put with non-valid stamps and data.
		   */
          MessagePut()
          {}

          /**
           * @brief create a MessagePut from a MessageWrite
           *
           * Convert the stamps to writable stamps, but get read-only buffer
           */
          MessagePut(const MessageWrite& m)
          : stamps(m.stamps), data(m.data)
          {}

          /// @}


          /// @name state
          /// @{
          /**
           * @brief  Clear the stamp list and the data buffer
           */
          void clear()
          {
              stamps.clear();
              data.clear();
          }
          
          /// @}

          /// @name operators
          /// @{
          /**
           * @brief compare this message to another MessagePut
           *
           * Two MessagePut instances are equal when stamps and data are equal
           *
           * @return
           *      - true when stamps equal m.stamps and data equals m.data
           *      - false else
           */
          bool operator==(const MessagePut& m) const
          {
              return stamps==m.stamps && data==m.data;
          }
          
          /**
           * @brief Message comparison operation
           */
          bool operator!=(const MessagePut& m) const
          {
              return !(*this == m);
          }
          /// @}

          StampsWrite stamps; /**< Message stamp list */
          Buffer      data;   /**<  Read-only data    */
  };


  /** 
   * @brief Basic flowvr read-only message.
   *
   * A message consists of stamps (information about the message / metadata / user-tagged data) and
   * the pay-load / data body.
   * A Message is typically received from an InputPort of a module, or resides in an input queue
   * of a filter or synchronizer.
   *
   * @see MessagePut, MessageWrite for interfaces that belong to the sender side or when you want
   *      to manipulate stamps or data as a filter.
   *
   * @ingroup Messagehandling
   */
  class Message
  {
      public:
		  /// @name construction and conversion
		  /// @{
          Message()
          {}

          /// @brief convert from a MessageWrite
          Message(const MessageWrite& m)
          : stamps(m.stamps), data(m.data)
          {}

          /// @brief convert from a MessagePut
          Message(const MessagePut& m)
          : stamps(m.stamps), data(m.data)
          {}

          /// @}

          /// @name introspection
          /// @{
          /**
           * @brief type ids for messages
           *
           * There are two types of message FULL or STAMPS
           */
          enum Type
          {
              STAMPS = 0, /**< Carry stamps only */
              FULL,       /**< Carry stamps  and data  */
              NBTYPES     /**< end-field id */
          };


          /**
           * @brief return the message type
           *
           * A message is a full message when the data section is valid (i.e., the message contains
           * data). A message thus always contains stamps.
           *
           * @return type id  (FULL or STAMPS)
           */
          Type getType() const
          {
              return data.valid()?FULL:STAMPS;
          }


          /**
           * @brief Test message validity 
           *
           * @return
           *     - true if stamps are valid
           *     - false else
           */
          bool valid() const
          {
              return stamps.valid();
          }

          /// @}

          /// @name state
          /// @{
          /**
           * @brief clear the message content (stamps and data)
           */
          void clear()
          {
              stamps.clear();
              data.clear();
          }
          /// @}


          /// @name operators
          /// @{
          /**
           * @brief compare to another message for equality
           *
           * Two messages are equal when data and stamps are equal
           *
           * @return
           *    - true when stamps and data are equal
           *    - false else
           */
          bool operator==(const Message& m) const
          {
              return stamps==m.stamps && data==m.data;
          }

                
          /**
           * @brief compare message for inequality
           *
           * Two messages are unequal when they are not equal (i.e.: either stamps
           * or data section are in mismatch)
           *
           * @return ! (*this == m)
           */
          bool operator!=(const Message& m) const
          {
              return !(*this == m);
          }

          /// @}


          Stamps stamps; /**< public read-only stamps */
          Buffer data;   /**< public read-only data */

          static const Message Null; /**<  Empty message instance for NULL references */
  };

} // namespace flowvr

#endif
