C-----------------------------------------------------------------------
!>
!> special a4_1hp_guess rwr
!>
!> EXECUTE A4_1HP_GUESS array0 array1 nroots
!>
!> Form the initial guess for 1-particle-hole (1hp) type (a,i) for whole set 
!> of methods. This routine analyze the diagonal of the 1hp matrix (input)
!> assign excitations (ie. set the weight to 1.0) based on the magnitude 
!> of the diagonals. This completely eliminate the old non-sensical guess 
!> generation that limits the number of guesses to hard coded values (and
!> the awakward nature that the guesses  have no relation to the actual 
!> matrix that is being inverted).
!> The array1 should contain 1hp matrix and the array2 returns guesses
!> for the number of roots (nroots). Arrays are assumed to be not distributed.
!> Ajith Perera, 04/2017.
C-----------------------------------------------------------------------

      subroutine A4_1hp_guess(
     * array_0, rank_0, index_values_0, size_0, extents_0, data_0,
     * array_1, rank_1, index_values_1, size_1, extents_1, data_1,
     * array_2, rank_2, index_values_2, size_2, extents_2, data_2,
     * ierr) BIND(C)

      use, intrinsic :: ISO_C_BINDING
      implicit none

      include 'sip_interface.f'
      include 'imemory.h'

C The input array that contain 1hp matrix

      integer(C_INT), intent(in)::array_0
      integer(C_INT), intent(in)::rank_0
      integer(C_INT), intent(in)::index_values_0(1:rank_0)
      integer(C_INT), intent(in)::size_0
      integer(C_INT), intent(in)::extents_0(1:rank_0)
      real(C_DOUBLE), intent(out)::data_0(1:size_0)

C This returns the guess vectors 

      integer(C_INT), intent(in)::array_1
      integer(C_INT), intent(in)::rank_1
      integer(C_INT), intent(in)::index_values_1(1:rank_1)
      integer(C_INT), intent(in)::size_1
      integer(C_INT), intent(in)::extents_1(1:rank_1)
      real(C_DOUBLE), intent(in)::data_1(1:size_1)

C The scalar the desinate number of roots 

      integer(C_INT), intent(in)::array_2
      integer(C_INT), intent(in)::rank_2
      integer(C_INT), intent(in)::index_values_2(1:rank_2)
      integer(C_INT), intent(in)::size_2
      integer(C_INT), intent(in)::extents_2(1:rank_2)
      real(C_DOUBLE), intent(in)::data_2(1:size_2)

      TYPE(C_PTR) :: cscr1, cscr2, cscr3
      integer(c_int) :: nscr1, nscr2, nscr3
      real(c_double), pointer :: scr1(:)
      real(c_double), pointer :: scr2(:)
      real(c_double), pointer :: scr3(:)

      integer(C_INT), intent(out)::ierr
      integer na1,na2,nb1,nb2,nb3,Inroots 
      integer Ndim,Mdim,Tdim

      real(C_DOUBLE) :: Dnroots 

      Dnroots = data_2(1)
      Inroots = INT(Dnroots)

      na1 = 1
      na2 = 1
      nb1 = 1
      nb2 = 1
      nb3 = 1

      if (.NOT. (rank_0 .eq. 2 .and. rank_1 .eq. 3)) then

         Write(6,"(a,a)") " The rank of the arrays must be 2 and 3", 
     &                    " in the suppe instruction A4_1hp_guess"
         call abort_job()

      endif

      Ndim = extents_0(1)
      Mdim = extents_0(2)
      Tdim = Ndim * Mdim 
      
      call scratch_array(Tdim,cscr1)
      call scratch_array(Inroots,cscr2)
      call scratch_array(Inroots,cscr3)

      call c_f_pointer(cscr1, scr1, [Ndim])
      call c_f_pointer(cscr2, scr2, [Inroots])
      call c_f_pointer(cscr3, scr3, [Inroots])

      call hp_guess(data_0,data_1,Inroots,Tdim,Ndim,Mdim,
     &              na1,extents_0(1),
     &              na2,extents_0(2),
     &              nb1,extents_1(1),
     &              nb2,extents_1(2),
     &              nb3,extents_1(3),
     &              scr1,scr2,scr3,ierr)

      call delete_scratch_array(cscr1)
      call delete_scratch_array(cscr2)
      call delete_scratch_array(cscr3)

      if (ierr .ne. 0) call abort_job()

      return
      end
C-------------------------------------------------------------------------

      Subroutine hp_guess(Diags,Gvecs,Inroots,Tdim,Ndim,Mdim,
     &                    l1,l2,m1,m2,n1,n2,o1,o2,p1,p2,Dtmp,
     &                    O_lable,V_label,ierr)

      include 'sip_interface.f'

      Integer l1,l2,m1,m2,n1,n2,o1,o2,p1,p2
      Integer Inroots,k
      Integer Tdim,Mdim 
      Integer Itmp,I,J,Pid
      Double Precision Xmin

      Double Precision Diags(l2-l1+1,m2-m1+1)
      Double Precision GVecs(n1:n2,o1:o2,p1:p2)
      Double Precision Scr1(l2-l1+1,m2-m1+1)
      Double Precision Dtmp(Ndim,Mdim)
      Integer O_Label(Inroots)
      Integer V_Label(Inroots)

      Pid = mpi_rank()

#ifdef _DEBUG_LVL0
      if (pid .eq. 0) then
      write(6,"(a,3i3)") "Input dimensions:", n2,o2,p2
      write(6,"(a)") "@-A4_1HP_GUESS @-entry, the diagonals"
      call output(Diags,1,n2,1,m2,n2,m2,1)
      endif
#endif

      Call Dcopy(Tdim, Diags, 1, Dtmp, 1)

      Do k = 1, Inroots

          Xmin = Diags(1,1)

          Do j= 1, Mdim
             If (j .Eq. 1) Then
                 Ibgn = 2
             Else
                 Ibgn = 1
             Endif
             Do i=Ibgn, Ndim
                If (Dtmp(I,J) .LT. Xmin) Then
                    Xmin  = Dtmp(I,J)
                    Itmp  = I
                    Jtmp  = J
                Endif
             Enddo
          Enddo

          O_label(k) = Jtmp
          V_label(k) = Itmp
          Dtmp(Itmp,Jtmp) = 1.0D10
      Enddo
#ifdef _DEBUG_LVL0
      if (pid .eq.0) then
      write(6,"(a)") "@-A4_1HP_GUESS The position of smallest digonals"
      write(6,"(6(1x,i3))") (O_label(i),i=1,Nroots)
      write(6,"(6(1x,i3))") (V_label(i),i=1,Nroots)
      endif
#endif

      Do k = 1, INroots
         Ondx = O_label(k)
         Vndx = V_label(k)
         GVecs(Vndx,Ondx,k) = 1.0D0
      Enddo

#ifdef _DEBUG_LVL0
      if (pid .eq.0) then
      write(6,"(a)") "@-A4_1HP_GUESS vectors"
      Do k = 1, INroots
          call output(GVecs(1,1,k),n1,n2,o1,o2,n2,o2,1)
      Enddo
      endif
#endif
      return
      end
