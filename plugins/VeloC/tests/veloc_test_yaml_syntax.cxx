/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * * Neither the name of CEA nor the names of its contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/

#include <pdi/testing.h>
#include <mpi.h>

using testing::Eq;
using testing::HasSubstr;

class VelocYamlConfig: public ::PDI::PdiTest
{};

TEST_F(VelocYamlConfig, ValidCustomConfiguration)
{
	MPI_Init(nullptr, nullptr);
	InitPdi(PC_parse_string((std::string(R"==(
metadata:
  ii: int
data:
  var: int
  veloc_file_buf: {type: array, subtype: char, size: 256}
plugins:
  veloc:
    config_file: )==") + VELOC_CONFIG_FILE + R"==(
    checkpoint_label: test_0
    iteration: ii
    custom_checkpointing:
      veloc_file : veloc_file_buf
      custom_checkpoint:
           filename: my_file.dat
           start_on_event: start_ckp
           route_file_on_event: route_ckp
           end_on_event: end_ckp
      custom_recover:
           filename: my_file.dat
           start_on_event: start_rec
           route_file_on_event: route_rec
           end_on_event: end_rec
)==").c_str()));

	FinalizePdi();
	MPI_Finalize();
}

/* ---- Top-level required-key errors ---- */

TEST_F(VelocYamlConfig, MissingConfigFile)
{
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_CONFIG), HasSubstr("VeloC configuration file is undefined")));

	InitPdi(PC_parse_string(R"==(
metadata:
  ii: int
data:
  var: int
plugins:
  veloc:
    checkpoint_label: test_0
    iteration: ii
)=="));
}

TEST_F(VelocYamlConfig, MissingCheckpointLabel)
{
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_CONFIG), HasSubstr("checkpoint label is undefined")));

	InitPdi(PC_parse_string(R"==(
metadata:
  ii: int
data:
  var: int
plugins:
  veloc:
    config_file: veloc_config.cfg
    iteration: ii
)=="));
}

TEST_F(VelocYamlConfig, MissingIteration)
{
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_CONFIG), HasSubstr("iteration number in the PDI data store is undefined")));

	InitPdi(PC_parse_string(R"==(
metadata:
  ii: int
data:
  var: int
plugins:
  veloc:
    config_file: veloc_config.cfg
    checkpoint_label: test_0
)=="));
}

TEST_F(VelocYamlConfig, IterationNotInProtectData)
{
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_CONFIG), HasSubstr("is not included in `protect_data'")));

	InitPdi(PC_parse_string(R"==(
metadata:
  ii: int
data:
  var: int
plugins:
  veloc:
    config_file: veloc_config.cfg
    checkpoint_label: test_0
    iteration: ii
    managed_checkpointing:
      protect_data: [var]
      checkpoint_on_event: ckp
)=="));
}

TEST_F(VelocYamlConfig, MissingProtectData)
{
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_CONFIG), HasSubstr("'protect_data' is undefined")));

	InitPdi(PC_parse_string(R"==(
metadata:
  ii: int
data:
  var: int
plugins:
  veloc:
    config_file: veloc_config.cfg
    checkpoint_label: test_01
    iteration: ii
    managed_checkpointing:
      checkpoint_on_event: ckp
)=="));
}

/* ---- custom_checkpoint sub-key errors ---- */

TEST_F(VelocYamlConfig, CustomCheckpointMissingVelocFile)
{
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_CONFIG), HasSubstr("'veloc_file' is undefined")));

	InitPdi(PC_parse_string(R"==(
metadata:
  ii: int
  veloc_file_buf: {type: array, subtype: char, size: 256}
data:
  var: int
plugins:
  veloc:
    config_file: veloc_config.cfg
    checkpoint_label: test_01
    iteration: ii
    custom_checkpointing:
       custom_checkpoint:
           filename: my_file.dat
           start_on_event: start_ckp
           route_file_on_event: route_ckp
           end_on_event: end_ckp
)=="));
}

TEST_F(VelocYamlConfig, CustomCheckpointMissingStartOnEvent)
{
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_CONFIG), HasSubstr("'start_on_event' is undefined")));

	InitPdi(PC_parse_string(R"==(
metadata:
  ii: int
  veloc_file_buf: {type: array, subtype: char, size: 256}
data:
  var: int
plugins:
  veloc:
    config_file: veloc_config.cfg
    checkpoint_label: test_01
    iteration: ii
    custom_checkpointing:
       veloc_file : veloc_file_buf
       custom_checkpoint:
           filename: my_file.dat
           route_file_on_event: route_ckp
           end_on_event: end_ckp
)=="));
}

TEST_F(VelocYamlConfig, CustomCheckpointMissingRouteFileOnEvent)
{
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_CONFIG), HasSubstr("'route_file_on_event' is undefined")));

	InitPdi(PC_parse_string(R"==(
metadata:
  ii: int
  veloc_file_buf: {type: array, subtype: char, size: 256}
data:
  var: int
plugins:
  veloc:
    config_file: veloc_config.cfg
    checkpoint_label: test_01
    iteration: ii
    custom_checkpointing:
       veloc_file : veloc_file_buf
       custom_checkpoint:
           filename: my_file.dat
           start_on_event: start_ckp
           end_on_event: end_ckp
)=="));
}

TEST_F(VelocYamlConfig, CustomCheckpointMissingEndOnEvent)
{
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_CONFIG), HasSubstr("'end_on_event' is undefined")));

	InitPdi(PC_parse_string(R"==(
metadata:
  ii: int
  veloc_file_buf: {type: array, subtype: char, size: 256}
data:
  var: int
plugins:
  veloc:
    config_file: veloc_config.cfg
    checkpoint_label: test_01
    iteration: ii
    custom_checkpointing:
       veloc_file : veloc_file_buf
       custom_checkpoint:
           filename: my_file.dat
           start_on_event: start_ckp
           route_file_on_event: route_ckp
)=="));
}

/* ---- custom_recover sub-key errors ---- */

TEST_F(VelocYamlConfig, CustomRecoverMissingFilename)
{
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_CONFIG), HasSubstr("'filename' is undefined")));

	InitPdi(PC_parse_string(R"==(
metadata:
  ii: int
  veloc_file_buf: {type: array, subtype: char, size: 256}
data:
  var: int
plugins:
  veloc:
    config_file: veloc_config.cfg
    checkpoint_label: test_01
    iteration: ii
    custom_checkpointing:
       veloc_file : veloc_file_buf
       custom_recover:
           start_on_event: start_ckp
           route_file_on_event: route_ckp
           end_on_event: end_ckp
)=="));
}

TEST_F(VelocYamlConfig, CustomRecoverMissingStartOnEvent)
{
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_CONFIG), HasSubstr("'start_on_event' is undefined")));

	InitPdi(PC_parse_string(R"==(
metadata:
  ii: int
  veloc_file_buf: {type: array, subtype: char, size: 256}
data:
  var: int
plugins:
  veloc:
    config_file: veloc_config.cfg
    checkpoint_label: test_01
    iteration: ii
    custom_checkpointing:
       veloc_file : veloc_file_buf
       custom_recover:
           filename: my_file.dat
           route_file_on_event: route_ckp
           end_on_event: end_ckp
)=="));
}

TEST_F(VelocYamlConfig, CustomRecoverMissingRouteFileOnEvent)
{
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_CONFIG), HasSubstr("'route_file_on_event' is undefined")));

	InitPdi(PC_parse_string(R"==(
metadata:
  ii: int
  veloc_file_buf: {type: array, subtype: char, size: 256}
data:
  var: int
plugins:
  veloc:
    config_file: veloc_config.cfg
    checkpoint_label: test_01
    iteration: ii
    custom_checkpointing:
      veloc_file: veloc_file_buf
      custom_recover:
        filename: my_file.dat
        start_on_event: start_rec
        end_on_event: end_rec
)=="));
}

TEST_F(VelocYamlConfig, CustomRecoverMissingEndOnEvent)
{
	EXPECT_CALL(*this, PdiError(Eq(PDI_ERR_CONFIG), HasSubstr("'end_on_event' is undefined")));

	InitPdi(PC_parse_string(R"==(
metadata:
  ii: int
  veloc_file_buf: {type: array, subtype: char, size: 256}
data:
  var: int
plugins:
  veloc:
    config_file: veloc_config.cfg
    checkpoint_label: test_01
    iteration: ii
    custom_checkpointing:
      veloc_file: veloc_file_buf
      custom_recover:
        filename: my_file.dat
        start_on_event: start_rec
        route_file_on_event: route_rec
)=="));
}

/* ---- Duplicate event names between checkpoint and recover ---- */

TEST_F(VelocYamlConfig, DuplicateEvents)
{
	EXPECT_CALL(
		*this,
		PdiError(Eq(PDI_ERR_CONFIG), HasSubstr("Duplicate event name"))
	);

	InitPdi(PC_parse_string(R"==(
metadata:
  ii: int
data:
  var: int
plugins:
  veloc:
    config_file: veloc_config.cfg
    checkpoint_label: test_0
    iteration: ii
    custom_checkpointing:
      custom_checkpoint:
           filename: my_file.dat
           start_on_event: start_ckp
           route_file_on_event: route_ckp
           end_on_event: end_ckp
      custom_recover:
           filename: my_file.dat
           start_on_event: start_ckp
           route_file_on_event: route_ckp
           end_on_event: end_ckp
)=="));
}