/* Generated By:JavaCC: Do not edit this line. TokenManager.h Version 6.0 */
/* JavaCCOptions:STATIC=false,SUPPORT_CLASS_VISIBILITY_PUBLIC=true */
#ifndef TOKENMANAGER_H
#define TOKENMANAGER_H
#include "JavaCC.h"
#include "Token.h"


namespace vhdl {
namespace parser {
/**
 * An implementation for this interface is generated by
 * JavaCCParser.  The user is free to use any implementation
 * of their choice.
 */

class TokenManager {
public:
  /** This gets the next token from the input stream.
   *  A token of kind 0 (<EOF>) should be returned on EOF.
   */
  public: virtual Token *getNextToken() = 0;
  public: virtual ~TokenManager() { }
  public: virtual void lexicalError() {
    fprintf(stderr, "Lexical error encountered.\n");
  }

};

}
}
#endif
/* JavaCC - OriginalChecksum=9e6cfa00cefe7e342b80eb59f1a114ff (do not edit this line) */