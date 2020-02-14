

poem = """
Babels of blocks to the high heavens towering
Flames of futility swirling below;
Poisonous fungi in brick and stone flowering,
Lanterns that shudder and death-lights that glow.

Black monstrous bridges across oily rivers,
Cobwebs of cable to nameless things spun;
Catacomb deeps whose dank chaos delivers
Streams of live foetor that rots in the sun.

Colour and splendour, disease and decaying,
Shrieking and ringing and crawling insane,
Rabbles exotic to stranger-gods praying,
Jumbles of odour that stifle the brain.

Legions of cats from the alleys nocturnal.
Howling and lean in the glare of the moon,
Screaming the future with mouthings infernal,
Yelling the Garden of Pluto's red rune.

Tall towers and pyramids ivy'd and crumbling,
Bats that swoop low in the weed-cumber'd streets;
Bleak Arkham bridges o'er rivers whose rumbling
Joins with no voice as the thick horde retreats.

Belfries that buckle against the moon totter,
Caverns whose mouths are by mosses effac'd,
And living to answer the wind and the water,
Only the lean cats that howl in the wastes.
"""

n_strophe = poem.count("\n") / 5

# pad lines to 50 characters
poem = "\n" + "\n".join([line.ljust(49) for line in poem.split("\n")])

import time, random
import flowvr

ports = flowvr.vectorPort()
port = flowvr.OutputPort('out')

ports.push_back(port)
module = flowvr.initModule(ports);


strophe_no = 0

while module.wait():
  time.sleep(3)


  print "say strophe", strophe_no

  message = flowvr.MessageWrite()
  strophe = poem[strophe_no * 50 * 5 : (strophe_no + 1) * 50 * 5]
  message.data = module.allocString(strophe)

  port.put(message)
  
  strophe_no += 1
  if strophe_no == n_strophe: strophe_no = 0

module.close()
