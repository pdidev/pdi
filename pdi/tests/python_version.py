#!/usr/bin/env python3
################################################################################
# Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# # Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# # Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# # Neither the name of CEA nor the names of its contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written permission.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
##############################################################################

import pdi

major, minor, patch = pdi.version()

assert pdi.version((0, 0, 0)) == (major, minor, patch), "no expected version is always compatible"

compatible = {
    (major, minor, patch): "exact match is compatible",
    (major, minor, 0): "patch is BW compatible",
    (major, minor, 255): "patch is FW compatible",
    (major, 0, patch): "minor is BW compatible",
    (major, 0, 0): "minor is BW compatible",
    (major, 0, 255): "minor is BW compatible",
}
for expected, reason in compatible.items():
    assert pdi.version(expected) == (major, minor, patch), f"{reason}: {expected}"

incompatible = {
    (major, 255, patch): "minor is not FW compatible",
    (major, 255, 0): "minor is not FW compatible",
    (major, 255, 255): "minor is not FW compatible",
    (0, minor, patch): "major is not BW compatible",
    (0, 0, 1): "major is not BW compatible",
    (0, 255, 255): "major is not BW compatible",
    (255, minor, patch): "major is not FW compatible",
    (255, 0, 0): "major is not FW compatible",
    (255, 255, 255): "major is not FW compatible",
    (major + 1, 0, 0): "major is not FW compatible",
}
for expected, reason in incompatible.items():
    try:
        pdi.version(expected)
    except pdi.Error as e:
        message = str(e)
        assert "PDI API version" in message, f"{reason}: {expected}: {message}"
        assert "{}.{}.{}".format(*expected) in message, f"{reason}: {expected}: {message}"
        assert f"{major}.{minor}.{patch}" in message, f"{reason}: {expected}: {message}"
        assert "%" not in message, f"{reason}: {expected}: {message}"
    else:
        raise AssertionError(f"{reason}: {expected} was accepted")
