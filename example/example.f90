PROGRAM example 

USE pdi

#define VAL2D(arr, xx, yy) (arr[(xx)+width*(yy)])



END PROGRAM example

MODULE funcUtil

  SUBROUTINE Init(data,width,height)
    REAL(8), DIMENSION(:,:), INTENT(INOUT) :: data
    INTEGER, INTENT(IN) :: width,height
  END SUBROUTINE Init

END MODULE funcUtil
