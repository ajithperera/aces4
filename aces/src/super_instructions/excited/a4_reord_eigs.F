C-----------------------------------------------------------------------
!>
!> special a4_reord_eigs uur 
!>
!> EXECUTE A4_REORD_EIGS array0 array1 array2
!> This SIP reorders eigenvalues and the corresponding eigenvectors ascending 
!> or decesnding order. 
!> The array0 and array1 are eigenvalues and the corresponding eigenvectors. 
!> The array2 is a flag to identify the order: Descending (0) and ascending
!> 1. Both arrays must be static. 
!> Ajith Perera, 04/2017.
C-----------------------------------------------------------------------

      subroutine A4_reord_eigs(
     * array_0, rank_0, index_values_0, size_0, extents_0, data_0,
     * array_1, rank_1, index_values_1, size_1, extents_1, data_1,
     * array_2, rank_2, index_values_2, size_2, extents_2, data_2,
     * ierr) BIND(C)

      use, intrinsic :: ISO_C_BINDING
      implicit none

      include 'sip_interface.f'
      include 'imemory.h'

C The input array that contain eigenvalues array.

      integer(C_INT), intent(in)::array_0
      integer(C_INT), intent(in)::rank_0
      integer(C_INT), intent(in)::index_values_0(1:rank_0)
      integer(C_INT), intent(in)::size_0
      integer(C_INT), intent(in)::extents_0(1:rank_0)
      real(C_DOUBLE), intent(out)::data_0(1:size_0)

C These contains the eigenvectors

      integer(C_INT), intent(in)::array_1
      integer(C_INT), intent(in)::rank_1
      integer(C_INT), intent(in)::index_values_1(1:rank_1)
      integer(C_INT), intent(in)::size_1
      integer(C_INT), intent(in)::extents_1(1:rank_1)
      real(C_DOUBLE), intent(in)::data_1(1:size_1)

C The flag indicating the prefered order

      integer(C_INT), intent(in)::array_2
      integer(C_INT), intent(in)::rank_2
      integer(C_INT), intent(in)::index_values_2(1:rank_2)
      integer(C_INT), intent(in)::size_2
      integer(C_INT), intent(in)::extents_2(1:rank_2)
      real(C_DOUBLE), intent(in)::data_2(1:size_2)

      integer(C_INT), intent(out)::ierr
      integer na1,na2,nb1,nb2,nc1,nc2
      integer Ndim,Mdim
      double precision Dflag 

      na1 = 1
      na2 = 1
      nb1 = 1
      nb2 = 1

      If (rank_1 .ne. 2) then

         Write(6,"(a,a)") " The rank of the array2 must be 2 in",
     &                    " A4_REORD_EIGS supper instruction "
         call abort_job()

      endif
      If (rank_0 .ne. 1) then

         Write(6,"(a,a)") " The rank of the array1 must be 1 in",
     &                    " A4_REORD_EIGS supper instruction "
         call abort_job()

      Endif 

      Ndim = extents_1(1)
      Mdim = extents_1(2)

      If (Ndim .Ne. Mdim) then
         Write(6,"(a,a)") " The matrix is rectangular",
     &                    " Not supported by A4_REORD_EIGS"
         call abort_job()
      Endif

      Dflag = Data_2(1)

      call Process_eigs(data_0,data_1,Dflag,Ndim,
     &                 na1,extents_0(1),
     &                 nb1,extents_1(1),
     &                 nb2,extents_1(2),
     &                 ierr)

      if (ierr .ne. 0) call abort_job()

      return
      end
C-------------------------------------------------------------------------

      Subroutine Process_eigs(Eigs,Evecs,DFlag,Ndim,
     &                        l1,l2,m1,m2,n1,n2,Ierr)

      
      Integer l1,l2,m1,m2,n1,n2,Ndim
      Integer Iflag

      Double Precision Eigs(l1:l2)
      Double Precision EVecs(m1:m2,n1:n2)
      Double Precision DFlag

#ifdef _DEBUG_LVL0
      If(me.eq.0) Then
      Write(6,"(a)") "Eigenvalues/vectors of incoming matrix"
      Write(6,"(6(1x,F10.5))") (Eigs(i),i=1,l2)
      call output(Evecs,m1,m2,n1,n2,m2,n2,1)
      endif
#endif
      Iflag = Int(Dflas)
    
      If (Iflag .Eq. 0) Then
         Call Reorder_d()
      Elseif (Iflag .Eq. 1) Then
         Call Reorder_a()
      Endif 

#ifdef _DEBUG_LVL0
      Write(6,"(a)") "Reordered Eigen values vectors"
      Write(6,"(6(1x,F10.5))") (Eigs(i),i=1,l2)
      call output(Evecs,m1,m2,n1,n2,m2,n2,1)
#endif

      Return
      End

      Subroutine Reorder_a(Eigs,Evecs,Ndim)

      Integer i,j,k,Ndim

      Double Precision Eigs(Ndim), Evecs(Ndim,Ndim)
      Double Precision Dtmp,DtmpR

C Reorder the eigenvalues and vectors Ascending order.
C Notice that this assumes that all the relavent eigenvalues are
C non-zero.

      If (Ndim .Eq. 1) Return

      Do i = 1, Ndim-1
         Do j = i+1, Ndim

          If (Eigs(j) .NE. 0.0D0) Then

          If (Eigs(i) .Gt. Eigs(j)) Then
              Dtmp    = Eigs(i)
              Eigs(i) = Eigs(j)
              Eigs(j) = Dtmp
              Do k = 1, Ndim
                 DtmpR      = Evecs(k,i)
                 Evecs(k,i) = Evecs(k,j)
                 Evecs(k,j) = DtmpR
              Enddo
          Endif
          Endif

         Enddo
      Enddo

      Return
      End

      Subroutine Reorder_d(Eigs,Evecs,Ndim)

      Integer i,j,Ndim,k

      Double Precision Eigs(Ndim), Evecs(Ndim,Ndim)
      Double Precision Dtmp,DtmpR

C Reorder the eigenvalues and vectors descending order.

      If (Ndim .Eq. 1) Return

      Do i = 1, Ndim-1
         Do j = i+1, Ndim

          If (Eigs(j) .NE. 0.0D0) Then

          If (Eigs(i) .Lt. Eigs(j)) Then
              Dtmp    = Eigs(i)
              Eigs(i) = Eigs(j)
              Eigs(j) = Dtmp
              Do k = 1, Ndim
                 DtmpR      = Evecs(k,i)
                 Evecs(k,i) = Evecs(k,j)
                 Evecs(k,j) = DtmpR
              Enddo
          Endif
          Endif

         Enddo
      Enddo

      Return
      End


