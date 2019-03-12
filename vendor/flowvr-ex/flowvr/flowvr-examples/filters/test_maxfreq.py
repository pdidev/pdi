

from flowvrapp import *
from filters import *


class Parrot(Module):
  """ sends out the text at random intervals """
  def __init__(self, name, text):
    Module.__init__(self, name, cmdline = "python modules/parrot.py " + text)
    self.addPort("out", direction = "out")

class Ear(Module):
  """ ouputs all messages it receives """
  def __init__(self, name):
    Module.__init__(self, name, cmdline = "python modules/ear.py")
    self.addPort("in", direction = "in")

# 1 parrot
parrot = Parrot("parrot", "hi!")

# 1 ear
ear = Ear("ear")


# pass messages through a greedy
greedy = Greedy("greedy")

# tests with other filter classes:
#
# FilterLastOrNull will drop all old messages
# greedy = Greedy("greedy", filter_class = FilterLastOrNull)
#
# FilterMergeIt will concatenate waiting messages
# greedy = Greedy("greedy", filter_class = FilterMergeIt)


parrot.getPort("out").link(greedy.getPort("in"))
greedy.getPort("out").link(ear.getPort("in"))

# control throughput of greedy with a MaxFrequency synchronizer

maxfreq = SyncMaxFrequency("maxfreq")
maxfreq.parameters["freq"] = 1  # frequency in Hz

ear.getPort("endIt").link(maxfreq.getPort("endIt"))
maxfreq.getPort("out").link(greedy.getPort("sync"))



app.generate_xml("test")
