C-----------------------------------------------------------------------
!>
!> special a4_return_select rru
!>
!> EXECUTE a4_return_select array0 array1 
!> This does the opposite of A4_select_max. ie. This return the value of 
!> an array correspond to the index. The array0 contains a vector of 
!> elements. The array1 sends in the number of elements (could be smaller
!> than the extents of arrray0). The array2 (scalar) returns the index of 
!> the maximum element in the array 1 corresponding to the incomming value
!> of array1.
!> Arrays must be static. 
!> Ajith Perera, 04/2017.
C-----------------------------------------------------------------------

      subroutine A4_return_select(
     * array_0, rank_0, index_values_0, size_0, extents_0, data_0,
     * array_1, rank_1, index_values_1, size_1, extents_1, data_1,
     * array_2, rank_2, index_values_2, size_2, extents_2, data_2,
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

C This is the current value of the number of elements <= extents_0

      integer(C_INT), intent(in)::array_1
      integer(C_INT), intent(in)::rank_1
      integer(C_INT), intent(in)::index_values_1(1:rank_1)
      integer(C_INT), intent(in)::size_1
      integer(C_INT), intent(in)::extents_1(1:rank_1)
      real(C_DOUBLE), intent(in)::data_1(1:size_1)

C This returns the position of the maximum element.

      integer(C_INT), intent(in)::array_2
      integer(C_INT), intent(in)::rank_2
      integer(C_INT), intent(in)::index_values_2(1:rank_2)
      integer(C_INT), intent(in)::size_2
      integer(C_INT), intent(in)::extents_2(1:rank_2)
      real(C_DOUBLE), intent(in)::data_2(1:size_2)

      integer(C_INT), intent(out)::ierr
      integer na1,Ndim,Index

      na1 = 1

      If (rank_0 .ne. 1) then

         Write(6,"(a,a)") " The rank of the array1 must be 1 in",
     &                    " A4_RETURN_SELECT supper instruction "
         call abort_job()

      endif

      Ndim  = INT(Data_1(1))
      Index = INT(data_2(1))

      call get_max_element(data_0,Index,data_2,na1,extents_0(1),Ndim,
     &                     ierr)

      if (ierr .ne. 0) call abort_job()
      
      return
      end
C-------------------------------------------------------------------------

      Subroutine get_max_element(A,Index,Value,l1,l2,Ndim,Ierr)
      
      include 'sip_interface.f'
      Integer l1,l2,Index,Ierr, Pid

      Double Precision A(l1:l2)
      Double Precision Value 

      Pid = mpi_rank()
     
#ifdef _DEBUG_LVL0
      if (Pid .Eq. 0) then
      write(6,"(a)") "@-A4_RETURN_SELECT; incoming array "
      Write(6,"(6(1x,F10.5))") (A(i),i=1,Ndim)
      endif 
#endif

      Value = A(Index) 
      Ierr = 0

#ifdef _DEBUG_LVL0
      if (pid .eq.0) then
      write(6,"(a,F10.5)") "The selected value:", Value
      endif
#endif
     
      Return
      End


