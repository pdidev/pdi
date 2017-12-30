/*******************************************************************************
 * Copyright (c) 2015, Corentin Roussel - CEA (corentin.roussel@cea.fr)
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

#include <assert.h>
#include <unistd.h>
#include <mpi.h>
#include <pdi.h>

int main(int argc, char *argv[])
{
  int value[5]= {5,4,3,2,1};
  int i;
  int *buf=NULL;
  const int nbuf=1000;
  double test_var=0;
  MPI_Init(&argc, &argv);
  assert(argc == 2 && "Needs 1 single arg: config file");
  PC_tree_t conf = PC_parse_path(argv[1]);
  MPI_Comm world = MPI_COMM_WORLD;
  PDI_init(conf, &world);

  PDI_transaction_begin("testing");
  PDI_expose("meta0",&value[0], PDI_OUT);
  PDI_expose("meta1",&value[0], PDI_OUT);
  buf = malloc(nbuf*sizeof(int)); // memory
  PDI_expose("meta2",&value[1], PDI_OUT);
  for(i=0; i<nbuf; i++) {
    buf[i]=0;
  }
  PDI_expose("meta3",&value[2], PDI_OUT);
  for(i=0; i<nbuf-1; i++) {
    buf[i]=buf[i+1]+1;
  }
  PDI_expose("meta4",&value[3], PDI_OUT);
  PDI_expose("test_var",&test_var, PDI_OUT);
  free(buf);
  PDI_transaction_end();
  PDI_finalize();

  FILE *fp = NULL;
  fp = fopen("test_01_variable_5.sion", "r");
  assert(fp != NULL  && "File not found.");
  fclose(fp);

  fp = fopen("test_01_event_5.sion", "r");
  assert(fp != NULL  && "File not found.");
  fclose(fp);

  PC_tree_destroy(&conf);
  MPI_Finalize();
  return 0;
}
