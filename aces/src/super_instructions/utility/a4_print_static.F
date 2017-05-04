C-----------------------------------------------------------------------
!>
!> special a4_print_static rrr 
!>
!> EXECUTE A4_PRINT_STATIC array0 array1
!> This utility prints a matrix or array in a neat format. Usefull 
!> in debugging  and other purposes. This may need to be genarizled 
!> to encompas all kinds of printing that we do. I hope this summer 
!> someone can wok on this. So, consider this SIP as a zeroth order 
!> version.
!> Ajith Perera, 04/2017.
C-----------------------------------------------------------------------

      subroutine A4_print_static(
     * array_0, rank_0, index_values_0, size_0, extents_0, data_0,
     * array_1, rank_1, index_values_1, size_1, extents_1, data_1,
     * array_2, rank_2, index_values_2, size_2, extents_2, data_2,
     * ierr) BIND(C)

      use, intrinsic :: ISO_C_BINDING
      implicit none

      include 'sip_interface.f'
      include 'imemory.h'

C The input array that contain any matrix

      integer(C_INT), intent(in)::array_0
      integer(C_INT), intent(in)::rank_0
      integer(C_INT), intent(in)::index_values_0(1:rank_0)
      integer(C_INT), intent(in)::size_0
      integer(C_INT), intent(in)::extents_0(1:rank_0)
      real(C_DOUBLE), intent(out)::data_0(1:size_0)

C The number of rows and columns 

      integer(C_INT), intent(in)::array_1
      integer(C_INT), intent(in)::rank_1
      integer(C_INT), intent(in)::index_values_1(1:rank_1)
      integer(C_INT), intent(in)::size_1
      integer(C_INT), intent(in)::extents_1(1:rank_1)
      real(C_DOUBLE), intent(in)::data_1(1:size_1)

      integer(C_INT), intent(in)::array_2
      integer(C_INT), intent(in)::rank_2
      integer(C_INT), intent(in)::index_values_2(1:rank_2)
      integer(C_INT), intent(in)::size_2
      integer(C_INT), intent(in)::extents_2(1:rank_2)
      real(C_DOUBLE), intent(in)::data_2(1:size_2)

      Integer Nrow,Ncol,na1,na2,Ierr
      Double Precision DNdim,DMdim

      na1 = 1
      na2 = 1

      DNdim = Data_1(1)
      DMdim = Data_2(1)

      Nrow = extents_0(1)
      If (INT(DMdim) .GT. 1) Then
         Ncol = extents_0(2)
      Else
         Ncol = 1
      Endif 

      If (Ncol .EQ. 1) Then
        call Neat_print(data_0,DNdim,DMdim,Nrow,Ncol,
     &                  na1,extents_0(1),
     &                  na2,Ncol,Ierr)
      Else 
        call Neat_print(data_0,DNdim,DMdim,Nrow,Ncol,
     &                 na1,extents_0(1),
     &                 na2,extents_0(2),Ierr)

      Endif 
      if (ierr .ne. 0) call abort_job()

      return
      end
C-------------------------------------------------------------------------

      Subroutine Neat_print(A,DNdim,DMdim,Nrow,Ncol,l1,l2,
     &                      m1,m2,Ierr)

      Integer l1,l2,m1,m2,n1,n2,o1,o2
      Integer Nrow,Ncol,ierr
      Double Precision DNdim,DMdim

      Double Precision A(l1:l2,m1:m2)
C
      Ndim = INT(DNdim)
      Mdim = INT(DMdim)

#ifdef _DEBUG_LVL0
      Write(6,"(a,4(1x,i2))") "Ndim,Mdim,Nrow,Ncol", 
     &                       Ndim,Mdim,Nrow,Ncol 
#endif 

      Call output(A,1,Ndim,1,Mdim,Nrow,Ncol,1)
      Ierr = 0

      Return
      End

      SUBROUTINE OUTPUT (MATRIX,ROWLOW,ROWHI,COLLOW,COLHI,ROWDIM,COLDIM,
     $                   NCTL)
C
C.......................................................................
C
C
C OUTPUT PRINTS A REAL*8 MATRIX IN FORMATTED FORM WITH NUMBERED ROWS
C
C AND COLUMNS.  THE INPUT IS AS FOLLOWS;
C
C        MATRIX(*,*).........MATRIX TO BE OUTPUT
C
C        ROWLOW..............ROW NUMBER AT WHICH OUTPUT IS TO BEGIN
C
C        ROWHI...............ROW NUMBER AT WHICH OUTPUT IS TO END
C
C        COLLOW..............COLUMN NUMBER AT WHICH OUTPUT IS TO BEGIN
C
C        COLHI...............COLUMN NUMBER AT WHICH OUTPUT IS TO END
C
C        ROWDIM..............ROW DIMENSION OF MATRIX(*,*)
C
C        COLDIM..............COLUMN DIMENSION OF MATRIX(*,*)
C
C        NCTL................CARRIAGE CONTROL FLAG; 1 FOR SINGLE SPACE
C                                                   2 FOR DOUBLE SPACE
C                                                   3 FOR TRIPLE SPACE
C
C THE PARAMETERS THAT FOLLOW MATRIX ARE ALL OF TYPE INTEGER*4.  THE
C
C PROGRAM IS SET UP TO HANDLE 5 COLUMNS/PAGE WITH A 1P5D24.15 FORMAT FOR
C
C THE COLUMNS.  IF A DIFFERENT NUMBER OF COLUMNS IS REQUIRED, CHANGE
C
C FORMATS 1000 AND 2000, AND INITIALIZE KCOL WITH THE NEW NUMBER OF
C
C COLUMNS.
C
C AUTHOR;  NELSON H.F. BEEBE, QUANTUM THEORY PROJECT, UNIVERSITY OF
C          FLORIDA, GAINESVILLE
C
C REVISED;  FEBRUARY 26, 1971
C
C Revised to work with f90 : 1/8/98. JDW.
C
C.......................................................................
CSW      1
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER ROWLOW,ROWHI,COLLOW,COLHI,ROWDIM,COLDIM,BEGIN,KCOL
CSW      1
      REAL*8 MATRIX(ROWDIM,COLDIM)
C.......................................................................
CSW      1
      CHARACTER*1 ASA,BLANK,CTL
      CHARACTER*6 COLUMN
      DIMENSION ASA(3)
      DATA COLUMN/'COLUMN'/,ASA/' ','0','-'/, BLANK/' '/
      DATA KCOL/4/
      DATA ZERO/0.D00/
      DO 10 I=ROWLOW,ROWHI
      DO 10 J=COLLOW,COLHI
      IF (MATRIX(I,J).NE.ZERO) GO TO 15
   10 CONTINUE
      WRITE (6,3000)
 3000 FORMAT ('0ZERO MATRIX.')
      GO TO 3
   15 CONTINUE
      CTL = BLANK
      IF ((NCTL.LE.3).AND.(NCTL.GT.0)) CTL = ASA(NCTL)
      IF (ROWHI.LT.ROWLOW) GO TO 3
      IF (COLHI.LT.COLLOW) GO TO 3
      LAST = MIN0(COLHI,COLLOW+KCOL-1)
      DO 2 BEGIN = COLLOW,COLHI,KCOL
      WRITE (6,1000) (COLUMN,I,I = BEGIN,LAST)
      DO 1 K = ROWLOW,ROWHI
      DO 4 I=BEGIN,LAST
CSSS      IF (MATRIX(K,I).NE.ZERO) GO TO 5
          GO TO 5
    4 CONTINUE
      GO TO 1
    5 WRITE (6,2000) CTL,K,(MATRIX(K,I), I = BEGIN,LAST)
    1 CONTINUE
    2 LAST = MIN0(LAST+KCOL,COLHI)
    3 RETURN
 1000 FORMAT (/,1X,16X,3(A6,I4,7X),(A6,I4))
 2000 FORMAT (A1,'ROW',I4,2X,4F17.11)
      END


