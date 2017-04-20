C-----------------------------------------------------------------------
!>
!> special a4_select_max rw
!>
!> EXECUTE a4_select_max array0 array1 
!> This SIP returns the position of the maximum element in an array.
!> The array0 contains a vector of elements. The array2 (scalar) 
!> returns the index of the maximum element of the array 1.
!> Array must be static. 
!> Ajith Perera, 04/2017.
C-----------------------------------------------------------------------

      subroutine A4_select_max(
     * array_0, rank_0, index_values_0, size_0, extents_0, data_0,
     * array_1, rank_1, index_values_1, size_1, extents_1, data_1,
     * ierr) BIND(C)

      use, intrinsic :: ISO_C_BINDING
      implicit none

      include 'sip_interface.f'
      include 'imemory.h'

C The input array that contain elements 

      integer(C_INT), intent(in)::array_0
      integer(C_INT), intent(in)::rank_0
      integer(C_INT), intent(in)::index_values_0(1:rank_0)
      integer(C_INT), intent(in)::size_0
      integer(C_INT), intent(in)::extents_0(1:rank_0)
      real(C_DOUBLE), intent(out)::data_0(1:size_0)

C This returns the position of the maximum element.

      integer(C_INT), intent(in)::array_1
      integer(C_INT), intent(in)::rank_1
      integer(C_INT), intent(in)::index_values_1(1:rank_1)
      integer(C_INT), intent(in)::size_1
      integer(C_INT), intent(in)::extents_1(1:rank_1)
      real(C_DOUBLE), intent(in)::data_1(1:size_1)

      integer(C_INT), intent(out)::ierr
      integer na1,Ndim

      na1 = 1

      If (rank_1 .ne. 1) then

         Write(6,"(a,a)") " The rank of the array1 must be 1 in",
     &                    " A4_SELECT_MAX supper instruction "
         call abort_job()

      endif

      Ndim = extents_1(1)

      call get_max(data_0,data_1,Ndim,na1,extents_0(1),ierr)

      if (ierr .ne. 0) call abort_job()

      return
      end
C-------------------------------------------------------------------------

      Subroutine get_max(A,dmax,Ndim,l1,l2,Ierr)
      
      Implicit None 
      Integer l1,l2,Imax,I,Ierr,Ndim

      Double Precision A(l1:l2)
      Double Precision Dmax 
     
#ifdef _DEBUG_LVL0
      write(6,"(a)") "@-A4_SELECT_MAX; incoming array "
      Write(6,"(6(1x,F10.5))") (Dabs(A(i))),i=1,l2)
#endif

      Imax = 1
      Do I = 1, Ndim
        if (Dabs(A(i)) .gt. Dabs(A(Imax))) then
            Imax = i
        Endif
      Enddo

#ifdef _DEBUG_LVL0
      write(6,"(a,i3)") "The position of the maximum element:",
     +                   Imax
#endif

      Dmax = Dble(Imax)
      Ierr = 0

      Return
      End


