MODULE arr_fill

USE mpi
USE kinds

IMPLICIT NONE


CONTAINS




SUBROUTINE fill_1d_dp(input_arr)
REAL(kind=DoubleReal), INTENT(INOUT) :: input_arr(:)
INTEGER(KIND=StandardInteger) :: n
DO n = 1, SIZE(input_arr)
  input_arr(n) = 1.0D0 * n
END DO
END SUBROUTINE fill_1d_dp


SUBROUTINE fill_2d_dp(input_arr)
REAL(kind=DoubleReal), INTENT(INOUT) :: input_arr(:,:)
INTEGER(KIND=StandardInteger) :: n, m
DO n = 1, SIZE(input_arr,2)
  DO m = 1, SIZE(input_arr,1)
    input_arr(m,n) = 1.0D0 * n * m
  END DO
END DO
END SUBROUTINE fill_2d_dp

SUBROUTINE fill_1d_int(input_arr)
INTEGER(KIND=StandardInteger), INTENT(INOUT) :: input_arr(:)
INTEGER(KIND=StandardInteger) :: n
DO n = 1, SIZE(input_arr)
  input_arr(n) = n
END DO
END SUBROUTINE fill_1d_int


SUBROUTINE fill_2d_int(input_arr)
INTEGER(KIND=StandardInteger), INTENT(INOUT) :: input_arr(:,:)
INTEGER(KIND=StandardInteger) :: n, m
DO n = 1, SIZE(input_arr,2)
  DO m = 1, SIZE(input_arr,1)
    input_arr(m,n) = n * m
  END DO
END DO
END SUBROUTINE fill_2d_int



END MODULE arr_fill