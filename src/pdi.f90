!/*******************************************************************************
! * Copyright (c) 2016, Julien Bigot | Mohamed Gaalich - CEA (julien.bigot@cea.fr)
! * All rights reserved.
! *
! * Redistribution and use in source and binary forms, with or without
! * modification, are permitted provided that the following conditions are met:
! * * Redistributions of source code must retain the above copyright
! *   notice, this list of conditions and the following disclaimer.
! * * Redistributions in binary form must reproduce the above copyright
! *   notice, this list of conditions and the following disclaimer in the
! *   documentation and/or other materials provided with the distribution.
! * * Neither the name of CEA nor the names of its contributors may be used to
! *   endorse or promote products derived from this software without specific 
! *   prior written permission.
! *
! * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
! * THE SOFTWARE.
! ******************************************************************************/
!MODULE PC_tree_type
!
!  use iso_C_binding

!  TYPE, bind(C) :: PC_tree_t_f  
!    INTEGER(C_INT) :: stat
!    TYPE(C_PTR) :: document
!    TYPE(C_PTR) :: node
!  END TYPE PC_tree_t_f

!END MODULE

MODULE pdi
  
  USE iso_C_binding
  USE paraconf
  
  IMPLICIT NONE 
  
  
  
  INTERFACE
    FUNCTION PDI_init_f(treeConf,world) &
      bind(C, name="PDI_init")   
      USE iso_C_binding 
      USE paraconf
      INTEGER(C_INT) :: PDI_Init_f
      TYPE(PC_tree_t_f), VALUE :: treeConf
      TYPE(C_PTR), VALUE :: world 
    END FUNCTION PDI_Init_f
  END INTERFACE
  
  INTERFACE  
    FUNCTION PDI_finalize_f() &
      bind(C, name="PDI_finalize")
      USE iso_C_binding 
      INTEGER(C_INT) :: PDI_finalize_f
    END FUNCTION PDI_finalize_f 
  END INTERFACE
  
  INTERFACE  
    FUNCTION PDI_event_f(event) &
      bind(C, name="PDI_event")
      USE iso_C_binding 
      INTEGER(C_INT) :: PDI_event_f
      TYPE(C_PTR), VALUE :: event
    END FUNCTION PDI_event_f 
  END INTERFACE
  
  INTERFACE  
    FUNCTION PDI_share_f(namef,dataf,accessf) &
      bind(C, name="PDI_share")
      USE iso_C_binding 
      INTEGER(C_INT) :: PDI_share_f
      TYPE(C_PTR), VALUE :: namef
      TYPE(C_PTR), VALUE :: dataf
      INTEGER(C_INT), VALUE :: accessf
    END FUNCTION PDI_share_f 
  END INTERFACE
  
  INTERFACE  
    FUNCTION PDI_release_f(namef) &
      bind(C, name="PDI_release")
      USE iso_C_binding 
      INTEGER(C_INT) :: PDI_release_f
      TYPE(C_PTR), VALUE :: namef
    END FUNCTION PDI_release_f 
  END INTERFACE

  INTERFACE  
    FUNCTION PDI_reclaim_f(namef) &
      bind(C, name="PDI_reclaim")
      USE iso_C_binding 
      INTEGER(C_INT) :: PDI_reclaim_f
      TYPE(C_PTR), VALUE :: namef
    END FUNCTION PDI_reclaim_f 
  END INTERFACE
  
  INTERFACE  
    FUNCTION PDI_export_f(namef, dataf) &
      bind(C, name="PDI_export")
      USE iso_C_binding 
      INTEGER(C_INT) :: PDI_export_f
      TYPE(C_PTR), VALUE :: namef
      TYPE(C_PTR), VALUE :: dataf
    END FUNCTION PDI_export_f 
  END INTERFACE
 
  INTERFACE  
    FUNCTION PDI_expose_f(namef, dataf) &
      bind(C, name="PDI_expose")
      USE iso_C_binding 
      INTEGER(C_INT) :: PDI_expose_f
      TYPE(C_PTR), VALUE :: namef
      TYPE(C_PTR), VALUE :: dataf
    END FUNCTION PDI_expose_f 
  END INTERFACE
  
  INTERFACE  
    FUNCTION PDI_import_f(namef, dataf) &
      bind(C, name="PDI_import")
      USE iso_C_binding 
      INTEGER(C_INT) :: PDI_import_f
      TYPE(C_PTR), VALUE :: namef
      TYPE(C_PTR), VALUE :: dataf
    END FUNCTION PDI_import_f 
  END INTERFACE
  
  INTERFACE PDI_share
    module procedure PDI_share_double_scalar
    module procedure PDI_share_int_scalar
  END INTERFACE PDI_share
  
  INTERFACE PDI_export
    module procedure PDI_export_double_scalar
    module procedure PDI_export_int_scalar 
  END INTERFACE PDI_export
  
  INTERFACE PDI_expose
    module procedure PDI_expose_double_scalar
    module procedure PDI_expose_int_scalar
    module procedure PDI_expose_double_array2D
  END INTERFACE PDI_expose
  
  INTERFACE PDI_import
    module procedure PDI_import_double_scalar
    module procedure PDI_import_int_scalar 
  END INTERFACE PDI_import
  
  CONTAINS
!=============================================================  
  SUBROUTINE PDI_init(treeConf,world,err)
    TYPE(PC_tree_t_f), INTENT(IN) :: treeConf
    INTEGER, INTENT(INOUT), TARGET :: world
    INTEGER, INTENT(OUT), OPTIONAL :: err
    
    INTEGER :: tmp
    
    if(PRESENT(err)) then
      err = int(PDI_init_f(treeConf,c_loc(world)))
    else
      tmp = int(PDI_init_f(treeConf,c_loc(world)))
    end if
  END SUBROUTINE PDI_init
!=============================================================
!=============================================================
  SUBROUTINE PDI_finalize(err)
    INTEGER, INTENT(OUT), OPTIONAL :: err
    
    INTEGER :: tmp
    if(PRESENT(err)) then
      err = int(PDI_finalize_f())
    else
      tmp = int(PDI_finalize_f())
    end if
  END SUBROUTINE PDI_finalize
!=============================================================
!=============================================================
  SUBROUTINE PDI_event(event,err)
    CHARACTER(LEN=*), INTENT(IN) :: event
    INTEGER, INTENT(OUT), OPTIONAL :: err
    
    CHARACTER(C_CHAR), TARGET :: C_event(len_trim(event)+1)
    INTEGER :: i,tmp
    
    do i=1,len_trim(event)
      C_event(i) = event(i:i)
    end do
    C_event(len_trim(event)+1) = C_NULL_CHAR
    
    if(PRESENT(err)) then
      err = int(PDI_event_f(c_loc(C_event)))
    else
      tmp = int(PDI_event_f(c_loc(C_event)))
    end if
  END SUBROUTINE PDI_event
!=============================================================
![[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
!============================================================= souci avec void *
  SUBROUTINE PDI_share_double_scalar(namef,dataf,accessf,err)
    CHARACTER(LEN=*), INTENT(IN) :: namef
    INTEGER, INTENT(IN) :: accessf
    INTEGER, INTENT(OUT), OPTIONAL :: err
    REAL(8), INTENT(INOUT), TARGET :: dataf
    
    CHARACTER(C_CHAR), TARGET :: C_namef(len_trim(namef)+1)
    INTEGER :: i,tmp
    
    do i=1,len_trim(namef)
      C_namef(i) = namef(i:i)
    end do
    C_namef(len_trim(namef)+1) = C_NULL_CHAR
    
    if(PRESENT(err)) then
      err = int(PDI_share_f(c_loc(C_namef),c_loc(dataf),accessf))
    else
      tmp = int(PDI_share_f(c_loc(C_namef),c_loc(dataf),accessf))
    end if
  END SUBROUTINE PDI_share_double_scalar
!=============================================================
!============================================================= souci avec void *
  SUBROUTINE PDI_share_int_scalar(namef,dataf,accessf,err)
    CHARACTER(LEN=*), INTENT(IN) :: namef
    INTEGER, INTENT(IN) :: accessf
    INTEGER, INTENT(OUT), OPTIONAL :: err
    INTEGER, INTENT(INOUT), TARGET :: dataf
    
    CHARACTER(C_CHAR), TARGET :: C_namef(len_trim(namef)+1)
    INTEGER :: i,tmp
    
    do i=1,len_trim(namef)
      C_namef(i) = namef(i:i)
    end do
    C_namef(len_trim(namef)+1) = C_NULL_CHAR
    
    if(PRESENT(err)) then
      err = int(PDI_share_f(c_loc(C_namef),c_loc(dataf),accessf))
    else
      tmp = int(PDI_share_f(c_loc(C_namef),c_loc(dataf),accessf))
    end if
  END SUBROUTINE PDI_share_int_scalar
!=============================================================
![[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
!=============================================================
  SUBROUTINE PDI_release(namef,err)
    CHARACTER(LEN=*), INTENT(IN) :: namef
    INTEGER, INTENT(OUT), OPTIONAL :: err
    
    CHARACTER(C_CHAR), TARGET :: C_namef(len_trim(namef)+1)
    INTEGER :: i,tmp
    
    do i=1,len_trim(namef)
      C_namef(i) = namef(i:i)
    end do
    C_namef(len_trim(namef)+1) = C_NULL_CHAR
    
    if(PRESENT(err)) then
      err = int(PDI_release_f(c_loc(C_namef)))
    else
      tmp = int(PDI_release_f(c_loc(C_namef)))
    end if
  END SUBROUTINE PDI_release
!=============================================================
!=============================================================
  SUBROUTINE PDI_reclaim(namef,err)
    CHARACTER(LEN=*), INTENT(IN) :: namef
    INTEGER, INTENT(OUT), OPTIONAL :: err
    
    CHARACTER(C_CHAR), TARGET :: C_namef(len_trim(namef)+1)
    INTEGER :: i,tmp
    
    do i=1,len_trim(namef)
      C_namef(i) = namef(i:i)
    end do
    C_namef(len_trim(namef)+1) = C_NULL_CHAR
    
    if(PRESENT(err)) then
      err = int(PDI_reclaim_f(c_loc(C_namef)))
    else
      tmp = int(PDI_reclaim_f(c_loc(C_namef)))
    end if
  END SUBROUTINE PDI_reclaim
!=============================================================
![[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
!============================================================= idem pour void *
  SUBROUTINE PDI_export_double_scalar(namef,dataf,err)
    CHARACTER(LEN=*), INTENT(IN) :: namef
    INTEGER, INTENT(OUT), OPTIONAL :: err
    REAL(8), INTENT(IN), TARGET :: dataf
    
    CHARACTER(C_CHAR), TARGET :: C_namef(len_trim(namef)+1)
    INTEGER :: i,tmp
    
    do i=1,len_trim(namef)
      C_namef(i) = namef(i:i)
    end do
    C_namef(len_trim(namef)+1) = C_NULL_CHAR
    
    if(PRESENT(err)) then
      err = int(PDI_export_f(c_loc(C_namef),c_loc(dataf)))
    else
      tmp = int(PDI_export_f(c_loc(C_namef),c_loc(dataf)))
    end if
  END SUBROUTINE PDI_export_double_scalar
!=============================================================
!============================================================= idem pour void *
  SUBROUTINE PDI_export_int_scalar(namef,dataf,err)
    CHARACTER(LEN=*), INTENT(IN) :: namef
    INTEGER, INTENT(OUT), OPTIONAL :: err
    INTEGER, INTENT(IN), TARGET :: dataf
    
    CHARACTER(C_CHAR), TARGET :: C_namef(len_trim(namef)+1)
    INTEGER :: i,tmp
    
    do i=1,len_trim(namef)
      C_namef(i) = namef(i:i)
    end do
    C_namef(len_trim(namef)+1) = C_NULL_CHAR
    
    if(PRESENT(err)) then
      err = int(PDI_export_f(c_loc(C_namef),c_loc(dataf)))
    else
      tmp = int(PDI_export_f(c_loc(C_namef),c_loc(dataf)))
    end if
  END SUBROUTINE PDI_export_int_scalar
!=============================================================
![[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
![[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
!============================================================= idem pour void *
  SUBROUTINE PDI_expose_double_scalar(namef,dataf,err)
    CHARACTER(LEN=*), INTENT(IN) :: namef
    INTEGER, INTENT(OUT), OPTIONAL :: err
    REAL(8), INTENT(IN), TARGET :: dataf
    
    CHARACTER(C_CHAR), TARGET :: C_namef(len_trim(namef)+1)
    INTEGER :: i,tmp
    
    do i=1,len_trim(namef)
      C_namef(i) = namef(i:i)
    end do
    C_namef(len_trim(namef)+1) = C_NULL_CHAR
    
    if(PRESENT(err)) then
      err = int(PDI_expose_f(c_loc(C_namef),c_loc(dataf)))
    else
      tmp = int(PDI_expose_f(c_loc(C_namef),c_loc(dataf)))
    end if
  END SUBROUTINE PDI_expose_double_scalar
!=============================================================
!============================================================= idem pour void *
  SUBROUTINE PDI_expose_int_scalar(namef,dataf,err)
    CHARACTER(LEN=*), INTENT(IN) :: namef
    INTEGER, INTENT(OUT), OPTIONAL :: err
    INTEGER, INTENT(IN), TARGET :: dataf
    
    CHARACTER(C_CHAR), TARGET :: C_namef(len_trim(namef)+1)
    INTEGER :: i,tmp
    
    do i=1,len_trim(namef)
      C_namef(i) = namef(i:i)
    end do
    C_namef(len_trim(namef)+1) = C_NULL_CHAR
    
    if(PRESENT(err)) then
      err = int(PDI_expose_f(c_loc(C_namef),c_loc(dataf)))
    else
      tmp = int(PDI_expose_f(c_loc(C_namef),c_loc(dataf)))
    end if
  END SUBROUTINE PDI_expose_int_scalar
!=============================================================
!============================================================= idem pour void *
  SUBROUTINE PDI_expose_double_array2D(namef,dataf,err)
    CHARACTER(LEN=*), INTENT(IN) :: namef
    INTEGER, INTENT(OUT), OPTIONAL :: err
    REAL(8),DIMENSION(:,:), INTENT(IN), POINTER :: dataf
    
    CHARACTER(C_CHAR), TARGET :: C_namef(len_trim(namef)+1)
    INTEGER :: i,tmp
    
    do i=1,len_trim(namef)
      C_namef(i) = namef(i:i)
    end do
    C_namef(len_trim(namef)+1) = C_NULL_CHAR
    
    if(PRESENT(err)) then
      err = int(PDI_expose_f(c_loc(C_namef),c_loc(dataf(lbound(dataf,1),lbound(dataf,2)))))
    else
      tmp = int(PDI_expose_f(c_loc(C_namef),c_loc(dataf(lbound(dataf,1),lbound(dataf,2)))))
    end if
  END SUBROUTINE PDI_expose_double_array2D
!=============================================================
![[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
![[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[
!============================================================= idem pour void *
  SUBROUTINE PDI_import_double_scalar(namef,dataf,err)
    CHARACTER(LEN=*), INTENT(IN) :: namef
    INTEGER, INTENT(OUT), OPTIONAL :: err
    REAL(8), INTENT(OUT), TARGET :: dataf
    
    CHARACTER(C_CHAR), TARGET :: C_namef(len_trim(namef)+1)
    INTEGER :: i,tmp
    
    do i=1,len_trim(namef)
      C_namef(i) = namef(i:i)
    end do
    C_namef(len_trim(namef)+1) = C_NULL_CHAR
    
    if(PRESENT(err)) then
      err = int(PDI_import_f(c_loc(C_namef),c_loc(dataf)))
    else
      tmp = int(PDI_import_f(c_loc(C_namef),c_loc(dataf)))
    end if
  END SUBROUTINE PDI_import_double_scalar
!=============================================================
!============================================================= idem pour void *
  SUBROUTINE PDI_import_int_scalar(namef,dataf,err)
    CHARACTER(LEN=*), INTENT(IN) :: namef
    INTEGER, INTENT(OUT), OPTIONAL :: err
    INTEGER, INTENT(OUT), TARGET :: dataf
    
    CHARACTER(C_CHAR), TARGET :: C_namef(len_trim(namef)+1)
    INTEGER :: i,tmp
    
    do i=1,len_trim(namef)
      C_namef(i) = namef(i:i)
    end do
    C_namef(len_trim(namef)+1) = C_NULL_CHAR
    
    if(PRESENT(err)) then
      err = int(PDI_import_f(c_loc(C_namef),c_loc(dataf)))
    else
      tmp = int(PDI_import_f(c_loc(C_namef),c_loc(dataf)))
    end if
  END SUBROUTINE PDI_import_int_scalar
!=============================================================
![[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[[  
  
  
  
 
END MODULE pdi
