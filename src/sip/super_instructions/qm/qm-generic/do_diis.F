C Copyright (c) 2003-2010 University of Florida  
C
C This program is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published by
C the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.

C This program is distributed in the hope that it will be useful,
C but WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
C GNU General Public License for more details.

C The GNU General Public License is included in this distribution
C in the file COPYRIGHT.
c--------------------------------------------------------------------------
!> special do_diis rurrr  
!> This compute the B matrix in DIIS procedure from the input error
!> vectors. Work for both 4 and 2 dimensional error vectors and 
!> for both UHF and RHF refrences. The arguments are, distributed 
!> array of the data, static array for B matrix, the iteration 
!> number and the diis_order (input variable)
!> 
c--------------------------------------------------------------------------

      subroutine do_diis(
     * array_0, rank_0, index_values_0, size_0, extents_0, data_0,
     * array_1, rank_1, index_values_1, size_1, extents_1, data_1,
     * array_2, rank_2, index_values_2, size_2, extents_2, data_2,
     * array_3, rank_3, index_values_3, size_3, extents_3, data_3,
     * array_4, rank_4, index_values_4, size_4, extents_4, data_4,
     * ierr) BIND(C)

      use, intrinsic :: ISO_C_BINDING
      implicit none
      include 'sip_interface.f'

! output array --> array_0
      integer(C_INT), intent(in)::array_0
      integer(C_INT), intent(in)::rank_0
      integer(C_INT), intent(in)::index_values_0(1:rank_0)
      integer(C_INT), intent(in)::size_0
      integer(C_INT), intent(in)::extents_0(1:rank_0)
      real(C_DOUBLE), intent(in)::data_0(1:size_0)

      integer(C_INT), intent(in)::array_1
! scalar array --> array_1
      integer(C_INT), intent(in)::rank_1
      integer(C_INT), intent(in)::index_values_1(1:rank_1)
      integer(C_INT), intent(in)::size_1
      integer(C_INT), intent(in)::extents_1(1:rank_1)
      real(C_DOUBLE), intent(out)::data_1(1:size_1)

! scalar array --> array_2
      integer(C_INT), intent(in)::array_2
      integer(C_INT), intent(in)::rank_2
      integer(C_INT), intent(in)::index_values_2(1:rank_2)
      integer(C_INT), intent(in)::size_2
      integer(C_INT), intent(in)::extents_2(1:rank_2)
      real(C_DOUBLE), intent(in)::data_2(1:size_2)

! scalar array --> array_3
      integer(C_INT), intent(in)::array_3
      integer(C_INT), intent(in)::rank_3
      integer(C_INT), intent(in)::index_values_3(1:rank_3)
      integer(C_INT), intent(in)::size_3
      integer(C_INT), intent(in)::extents_3(1:rank_3)
      real(C_DOUBLE), intent(in)::data_3(1:size_3)

! scalar array --> array_4
      integer(C_INT), intent(in)::array_4
      integer(C_INT), intent(in)::rank_4
      integer(C_INT), intent(in)::index_values_4(1:rank_4)
      integer(C_INT), intent(in)::size_4
      integer(C_INT), intent(in)::extents_4(1:rank_4)
      real(C_DOUBLE), intent(in)::data_4(1:size_4)

      integer(C_INT), intent(out)::ierr
c mosegments 

      TYPE(C_PTR) :: cptr_n_moa_segs, cptr_moa_segs
      integer, pointer :: fptr_moa_segs(:), fptr_n_moa_segs(:)

      TYPE(C_PTR) :: cptr_n_mob_segs, cptr_mob_segs
      integer, pointer :: fptr_mob_segs(:), fptr_n_mob_segs(:)

      integer(C_INT) offset_1a, offset_2a, offset_3a, offset_4a
      integer(C_INT) offset_1b, offset_2b, offset_3b, offset_4b

      integer(C_INT) i, i1, i2, i3, i4, one, iflag, Rank
      integer(C_INT) Ncycle, Diis_order

      ierr = 0
      Write(6,*) "extents_05", extents_0(5), rank_0 

      Ncycle     = INT(Data_2(1))
      Diis_order = INT(Data_3(1))
      Iflag      = INT(data_4(1))
      Rank       = Rank_0 - 1 
C
C If the rank of the incomming array is five (for example, E(II,AB,Iter)
C E(Ij,AB,Iter), E(ij,ab.Iter)) then the error vector is a 4 index 
C quantity. 
C

      if (Rank .eq. 4) then 
   
         If (Iflag .EQ. 1) then

C The incoming arrays are AAAA type et the mo segment ranges

           call predefined_int_array("moa_seg_ranges"//C_NULL_CHAR, 
     &                                one,cptr_n_moa_segs, 
     &                                cptr_moa_segs)
           call c_f_pointer(cptr_n_moa_segs,fptr_n_moa_segs,[1])
           call c_f_pointer(cptr_moa_segs,fptr_moa_segs,
     &                      fptr_n_moa_segs)

           offset_1a = 0
           offset_2a = 0
           offset_3a = 0
           offset_4a = 0
           if (index_values_0(1).gt.1) then
              do i = 1, index_values_0(1) - 1
                 offset_1a = offset_1a + fptr_moa_segs(i)
              enddo
           endif
           if (index_values_0(2).gt.1) then
              do i = 1, index_values_0(2) - 1
                 offset_2a = offset_2a + fptr_moa_segs(i)
              enddo
          endif
          if (index_values_0(3).gt.1) then
             do i = 1, index_values_0(3) - 1
                offset_3a = offset_3a + fptr_moa_segs(i)
             enddo
          endif
          if (index_values_0(4).gt.1) then
             do i = 1, index_values_0(4) - 1
                offset_4a = offset_4a + fptr_moa_segs(i)
             enddo
          endif

          call do_diis_xxxx(1, extents_0(1),offset_1a,
     *                      1, extents_0(2),offset_2a,
     *                      1, extents_0(3),offset_3a,
     *                      1, extents_0(4),offset_4a,
     *                      1, extents_0(5), 
     *                      data_0,
     *
     *                      1, extents_1(1),  1, extents_1(2),
     *
     *                      Ncycle, diis_order, 
     *
     *                      data_1)


         Elseif (Iflag .EQ. 2) then

C The incoming arrays are BBBB type et the mo segment ranges

           call predefined_int_array("mob_seg_ranges"//C_NULL_CHAR, 
     &                                one,cptr_n_mob_segs, 
     &                                cptr_mob_segs)
           call c_f_pointer(cptr_n_mob_segs,fptr_n_mob_segs,[1])
           call c_f_pointer(cptr_mob_segs,fptr_mob_segs,
     &                      fptr_n_mob_segs)

           offset_1b = 0
           offset_2b = 0
           offset_3b = 0
           offset_4b = 0
           if (index_values_0(1).gt.1) then
              do i = 1, index_values_0(1) - 1
                 offset_1b = offset_1b + fptr_mob_segs(i)
              enddo
           endif
           if (index_values_0(2).gt.1) then
              do i = 1, index_values_0(2) - 1
                 offset_2b = offset_2b + fptr_mob_segs(i)
              enddo
          endif
          if (index_values_0(3).gt.1) then
             do i = 1, index_values_0(3) - 1
                offset_3b = offset_3b + fptr_mob_segs(i)
             enddo
          endif
          if (index_values_0(4).gt.1) then
             do i = 1, index_values_0(4) - 1
                offset_4b = offset_4b + fptr_mob_segs(i)
             enddo
          endif

          call do_diis_xxxx(1, extents_0(1),offset_1b,
     *                      1, extents_0(2),offset_2b,
     *                      1, extents_0(3),offset_3b,
     *                      1, extents_0(4),offset_4b,
     *                      1, extents_0(5), 
     *                      data_0,
     *
     *                      1, extents_1(1),  1, extents_1(2),
     * 
     *                      Ncycle, diis_order, 
     *
     *                      data_1)

         Elseif (Iflag .EQ. 3) then

C The incoming arrays are ABAB type and get the mo segment ranges.

           call predefined_int_array("moa_seg_ranges"//C_NULL_CHAR,
     &                                one,cptr_n_moa_segs,
     &                                cptr_moa_segs)
           call c_f_pointer(cptr_n_moa_segs,fptr_n_moa_segs,[1])
           call c_f_pointer(cptr_moa_segs,fptr_moa_segs,
     &                      fptr_n_moa_segs)

           call predefined_int_array("mob_seg_ranges"//C_NULL_CHAR, 
     &                                one,cptr_n_mob_segs, 
     &                                cptr_mob_segs)
           call c_f_pointer(cptr_n_mob_segs,fptr_n_mob_segs,[1])
           call c_f_pointer(cptr_mob_segs,fptr_mob_segs,
     &                      fptr_n_mob_segs)
           offset_1a = 0
           offset_2a = 0
           offset_1b = 0
           offset_2b = 0
           if (index_values_0(1).gt.1) then
              do i = 1, index_values_0(1) - 1
                 offset_1a = offset_1a + fptr_moa_segs(i)
              enddo
           endif
           if (index_values_0(2).gt.1) then
              do i = 1, index_values_0(2) - 1
                 offset_2a = offset_2a + fptr_moa_segs(i)
              enddo
          endif
          if (index_values_0(3).gt.1) then
             do i = 1, index_values_0(3) - 1
                offset_1b = offset_1b + fptr_mob_segs(i)
             enddo
          endif
          if (index_values_0(4).gt.1) then
             do i = 1, index_values_0(4) - 1
                offset_2b = offset_2b + fptr_mob_segs(i)
             enddo
          endif

          call do_diis_xxxx(1, extents_0(1),offset_1a,
     *                      1, extents_0(2),offset_2a,
     *                      1, extents_0(3),offset_1b,
     *                      1, extents_0(4),offset_2b,
     *                      1, extents_0(5),
     *                      data_0,
     *
     *                      1, extents_1(1),  1, extents_1(2),
     * 
     *                      Ncycle, diis_order, 
     *
     *                      data_1) 

        Endif
C
C If the rank of the incomming array is 3 (for example, E(A,B,Iter)
C E(ab.Iter)) then the error vector is a 2 index quantity. 
C

      ELseif (Rank .eq. 2) then

         If (Iflag .EQ. 1) then

C The incoming arrays are AA type. get the mo segment ranges

           call predefined_int_array("moa_seg_ranges"//C_NULL_CHAR,
     &                                one,cptr_n_moa_segs,
     &                                cptr_moa_segs)
           call c_f_pointer(cptr_n_moa_segs,fptr_n_moa_segs,[1])
           call c_f_pointer(cptr_moa_segs,fptr_moa_segs,
     &                      fptr_n_moa_segs)

           offset_1a = 0
           offset_2a = 0
           offset_3a = 0
           offset_4a = 0
           if (index_values_0(1).gt.1) then
              do i = 1, index_values_0(1) - 1
                 offset_1a = offset_1a + fptr_moa_segs(i)
              enddo
           endif
           if (index_values_0(2).gt.1) then
              do i = 1, index_values_0(2) - 1
                 offset_2a = offset_2a + fptr_moa_segs(i)
              enddo
          endif

          call do_diis_xx(1, extents_0(1),offset_1a,
     *                    1, extents_0(2),offset_2a,
     *                    1, extents_0(3),
     *                    data_0,
     *
     *                    1, extents_1(1),  1, extents_1(2),
     * 
     *                    Ncycle, diis_order, 
     *
     *                    data_1)

         Elseif (Iflag .EQ. 2) then
  
C The incoming arrays are BB type and get the mo segment ranges

           call predefined_int_array("mob_seg_ranges"//C_NULL_CHAR,
     &                                one,cptr_n_mob_segs,
     &                                cptr_mob_segs)
           call c_f_pointer(cptr_n_mob_segs,fptr_n_mob_segs,[1])
           call c_f_pointer(cptr_mob_segs,fptr_mob_segs,
     &                      fptr_n_mob_segs)

           offset_1b = 0
           offset_2b = 0
           if (index_values_0(1).gt.1) then
              do i = 1, index_values_0(1) - 1
                 offset_1b = offset_1b + fptr_mob_segs(i)
              enddo
           endif
           if (index_values_0(2).gt.1) then
              do i = 1, index_values_0(2) - 1
                 offset_2b = offset_2b + fptr_mob_segs(i)
              enddo
          endif

          call do_diis_xx(1, extents_0(1),offset_1b,
     *                    1, extents_0(2),offset_2b,
     *                    1, extents_0(3),
     *                    data_0,
     *
     *                    1, extents_1(1), 1, extents_1(2),
     *
     *                    Ncycle, diis_order, 
     *
     *                    data_1)

        Endif 

      Endif 

      return
      end
C
C-----------------------------------------------------------------------
C

      subroutine do_diis_xxxx(a1, a2, off_1, b1, b2, off_2, 
     *                        c1, c2, off_3, d1, d2, off_4, 
     *                        e1, e2, 
     *                     
     *                        array,
     *
     *                        m1, n1, m2, n2, 
     *
     *                        ncycle, diis_order,
     *
     *                        B)
      
      implicit none
      integer a1, a2, b1, b2, c1, c2, d1, d2, m1, m2, e1, e2
      integer n1, n2, i, j
      integer off_1, off_2, off_3, off_4 
      integer Adim, Bdim, Cdim, Ddim, Edim, Ndim
      integer ncycle, diis_order
      double precision array(a1:a2,b1:b2,c1:c2,d1:d2,e1:e2) 
      double precision B(m1:n1,m2:n2)
      double precision overlap, Ddot 

      Adim = (a2 - a1) + 1
      Adim = (b2 - b1) + 1
      Bdim = (b2 - b1) + 1
      Cdim = (b2 - b1) + 1
      Ddim = (b2 - b1) + 1
      Edim = (e2 - e1) + 1
       
      Ndim = Adim * Bdim * Cdim * Ddim 

C#ifdef _DEBUG_LVL0
      Write(6,"(a,8(1x,i4))") "Ndim,a1,a2,b1,b2,c1,c2,d1,d2,e1,e2",
     &                         Ndim,a1,a2,b1,b2,c1,c2,d1,d2,e1,e2,
     &                         m1,n1,m2,n2
C#endif 

      If (Ncycle .EQ. 1) Then

          Overlap = Ddot(Ndim, array(a1,b1,c1,d1,1), 1,
     &                         array(a1,b1,c1,d1,1), 1)
          B(1,1) = Overlap
C#ifdef _DEBUG_LVL0
        write(6, "(a,I4)") " The number of cycles and B-Mat:", Ncycle
        Write(6, "(F10.8)") B(1,1)
C#endif 
          Return

      Endif 

      If (Ncycle .LT. diis_order) Then

         Do I = 1, Ncycle 
            Do J = 1, I
        
               Overlap = Ddot(Ndim, array(a1,b1,c1,d1,(e1+I-1)), 1, 
     &                              array(a1,b1,c1,d1,(e1+J-1)), 1)
               B(I,J) = Overlap 

            Enddo 
          Enddo 
           
          Do I = 1, Ncycle
             Do J = 1, I-1
                B(J,I) = B(I,J)
             Enddo 
          Enddo

C#ifdef _DEBUG_LVL0
        write(6, "(a,I4)") " The number of cycles and B-Mat:", Ncycle
        Write(6,"(a)") "The 4-index block 1"
        Write(6, "(6(1x,F10.8))") ((B(i,j),i=1,Ncycle),j=1,Ncycle)
C#endif 

      Else 

         Do I = 1, Diis_order 
            Do J = 1, I
        
               Overlap = Ddot(Ndim, array(a1,b1,c1,d1,(e1+I-1)), 1, 
     &                              array(a1,b1,c1,d1,(e1+J-1)), 1)
               B(I,J) = Overlap 

            Enddo 
          Enddo 

          Do I = 1, Diis_order
             Do J = 1, I-1
                B(J,I) = B(I,J)
             Enddo 
          Enddo

C#ifdef _DEBUG_LVL0
        write(6, "(a,I4)") " The number of cycles and B-Mat:", Ncycle
        Write(6,"(a)") "The 4-index block 2"
        Write(6, "(6(1x,F10.8))") ((B(i,j),i=1,diis_order),
     &                              j=1,diis_order)
C#endif 

      Endif 

      return
      end


      subroutine do_diis_xx(a1, a2, off_1, b1, b2, off_2, e1, e2,
     *
     *                      array, 
     *
     *                      m1, n1, m2, n2, 
     * 
     *                      ncycle, diis_order,
     *
     *                      B)

      integer a1, a2, b1, b2
      integer m1, m2, n1, n2, i, j
      integer off_1, off_2
      integer Adim, Bdim, Edim, Ndim
      integer ncycle, diis_order
      double precision array(a1:a2,b1:b2,e1:e2)
      double precision B(m1:n1,m2:n2)
      double precision overlap, Ddot 

      Adim = (a2 - a1) + 1
      Bdim = (b2 - b1) + 1
      Edim = (e2 - e1) + 1

      Ndim = Adim * Bdim 

      If (Ncycle .EQ. 1) Then

          Overlap = Ddot(Ndim, array(a1,b1,1), 1,
     &                         array(a1,b1,1), 1)
          B(1,1) = Overlap
          Return

      Endif

      If (Ncycle .LT. diis_order) Then

         Do I = 1, Ncycle
            Do J = 1, I

               Overlap = Ddot(Ndim, array(a1,b1,(e1+I-1)), 1,
     &                              array(a1,b1,(e1+J-1)), 1)
               B(I,J) = Overlap

            Enddo
          Enddo

          Do I = 1, Ncycle
             Do J = 1, I-1
                B(J,I) = B(I,J)
             Enddo
          Enddo

C#ifdef _DEBUG_LVL0
        write(6, "(a,I4)") " The number of cycles and B-Mat:", Ncycle
        Write(6,"(a)") "The 2-index block 1"
        Write(6, "(6(1x,F10.8))") ((B(i,j),i=1,Ncycle),j=1,Ncycle)
C#endif 

      Else

         Do I = 1, Diis_order
            Do J = 1, I

               Overlap = Ddot(Ndim, array(a1,b1,(e1+I-1)), 1,
     &                              array(a1,b1,(e1+J-1)), 1)
               B(I,J) = Overlap

            Enddo
          Enddo

          Do I = 1, Ncycle
             Do J = 1, I-1
                B(J,I) = B(I,J)
             Enddo
          Enddo

C#ifdef _DEBUG_LVL0
        write(6, "(a,I4)") " The number of cycles and B-Mat:", Ncycle
        Write(6,"(a)") "The 2-index block 2"
        Write(6, "(6(1x,F10.8))") ((B(i,j),i=1,Ncycle),j=1,Ncycle)
C#endif 

      Endif 

      return
      end

