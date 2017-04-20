C-----------------------------------------------------------------------
!>
!> special a4_xgeev uu 
!>
!> EXECUTE A4_XGEEV array0 array1 array2
!> 
!> This is simply aces4 interface to the lapack non-symmetric diagonalization
!> routine dgeev. In principle gen_eigen_calc.F suppose to do the same thing, 
!> but I have no idea about the mess that is in there. This does one and only
!> one thing: that is to call lapack dgeev and return eigenvalues and 
!> eigenvectors; nothing more, nothing less. The array0 contains the
!> incomming matrix, diagonal of which eventually get filled by eigenvalues
!> and the array1 and array2 return right and left eigenvectors. Both arrays
!> must be static. 
!> Ajith Perera, 04/2017.
C-----------------------------------------------------------------------

      subroutine A4_xgeev(
     * array_0, rank_0, index_values_0, size_0, extents_0, data_0,
     * array_1, rank_1, index_values_1, size_1, extents_1, data_1,
     * array_2, rank_2, index_values_2, size_2, extents_2, data_2,
     * ierr) BIND(C)

      use, intrinsic :: ISO_C_BINDING
      implicit none

      include 'sip_interface.f'
      include 'imemory.h'

C The input array that contain any matrix; eigenvalues 
C are returned in the diagonal.

      integer(C_INT), intent(in)::array_0
      integer(C_INT), intent(in)::rank_0
      integer(C_INT), intent(in)::index_values_0(1:rank_0)
      integer(C_INT), intent(in)::size_0
      integer(C_INT), intent(in)::extents_0(1:rank_0)
      real(C_DOUBLE), intent(out)::data_0(1:size_0)

C These returns the right and left eigenvectors

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

      TYPE(C_PTR) :: cscr1, cscr2, cscr3, cscr4, cscr5
      integer(c_int) :: nscr1, nscr2, nscr3, nscr4, ncscr5
      real(c_double), pointer :: scr1(:)
      real(c_double), pointer :: scr2(:)
      real(c_double), pointer :: scr3(:)

      integer(C_INT), intent(out)::ierr
      integer na1,na2,nb1,nb2,nc1,nc2
      integer Ndim,Mdim,Tdim

      na1 = 1
      na2 = 1
      nb1 = 1
      nb2 = 1
      nc1 = 1
      nc2 = 1

      if (.NOT. (rank_0 .eq. 2 .and. rank_1 .eq. 2)) then

         Write(6,"(a,a)") " The rank of the array must be 2 in",
     &                    " supper instruction a4_xgeev.F"
         call abort_job()

      endif

      Ndim = extents_0(1)
      Mdim = extents_0(2)

      If (Ndim .Ne. Mdim) then
         Write(6,"(a,a)") " The matrix is rectangular",
     &                    " Not supported by XGEEV:consider SVD"
         call abort_job()
      Endif

      call scratch_array(Ndim,cscr1)
      call scratch_array(Ndim,cscr2)
      call scratch_array(4*Ndim,cscr3)

      call c_f_pointer(cscr1, scr1, [Ndim])
      call c_f_pointer(cscr2, scr2, [Ndim])
      call c_f_pointer(cscr3, scr3, [Ndim])

      call Diagonalize(data_0,data_1,data_2,Ndim,
     &                 na1,extents_0(1),
     &                 na2,extents_0(2),
     &                 nb1,extents_1(1),
     &                 nb2,extents_1(2),
     &                 nc1,extents_2(1),
     &                 nc2,extents_2(2),
     &                 scr1,scr2,scr3,ierr)

      call delete_scratch_array(cscr1)
      call delete_scratch_array(cscr2)
      call delete_scratch_array(cscr3)
      call delete_scratch_array(cscr5)

      if (ierr .ne. 0) call abort_job()

      return
      end
C-------------------------------------------------------------------------

      Subroutine Diagonalize(A,EvecR,EvecL,Ndim,l1,l2,m1,m2,
     &                       n1,n2,o1,o2,p1,p2,q1,q2,
     &                       EigR,EigI,Scr,Ierr)

      Integer l1,l2,m1,m2,n1,n2,o1,o2
      Integer Ndim,ierr

      Double Precision A(l1:l2,m1:m2)
      Double Precision EvecR(n1:n2,o1:o2)
      Double Precision EVecL(p1:p2,q1:q2)
      Double Precision EvalR(ndim)
      Double Precision EvalI(ndim)
      Double Precision Scr(4*ndim)

#ifdef _DEBUG_LVL0
      write(6,"(a,3i3)") "Input dimensions:", l2,m2,ndim
      write(6,"(a)") "@-A4_XGEEV the incoming matrix"
      call output(A,1,l2,1,m2,l2,m2,1)
#endif
      Call Dgeev("V","V",Ndim,A,Ndim,EvalR,EvalI,EvecL,Ndim,
     +            EvecR,Ndim,Scr,4*Ndim,Ierr)
C
      Call Zero(A,Ndim*Ndim)
      Do i = 1, Ndim
         A(i,i) = EvalR(i)
      Enddo

#ifdef _DEBUG_LVL0
      Write(6,"(a,a)") "@-A4_XGEEV Eigenvalues/vectors of the",
     +                 " incoming matrix"
      Write(6,"(a)") "The right-hand side"
      Write(6,"(6(1x,F10.5))") (EigR(i),i=1,k)
      Write(6,"(a)") "The imaginary eigenvalues"
      Write(6,"(6(1x,F10.5))") (EigI(i),i=1,k)
      call output(EvecR,1,k,1,k,Ndim,Ndim,1)
      Write(6,"(a)") "The left-hand side"
      call output(EvecL,1,k,1,k,Ndim,Ndim,1)
#endif

      Return
      End

      Subroutine Zero(A, Len)
      Implicit None
      Double Precision A(Len)
      Integer I, Len

      If (Len .Eq. 0) Return
      Do I = 1, Len
         A(i) = 0.0D0
      Enddo

      Return
      End


