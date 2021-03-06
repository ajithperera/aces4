C  Copyright (c) 2003-2010 University of Florida
C
C  This program is free software; you can redistribute it and/or modify
C  it under the terms of the GNU General Public License as published by
C  the Free Software Foundation; either version 2 of the License, or
C  (at your option) any later version.

C  This program is distributed in the hope that it will be useful,
C  but WITHOUT ANY WARRANTY; without even the implied warranty of
C  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C  GNU General Public License for more details.

C  The GNU General Public License is included in this distribution
C  in the file COPYRIGHT.
      subroutine compute_nn_repulsion(
     * array_slot_0, rank_0, index_values_0, size_0, extents_0, data_0, 
     * ierr) BIND(C) 
       use, intrinsic :: ISO_C_BINDING
      implicit none
      include 'sip_interface.f'  
c---------------------------------------------------------------------------
!> special compute_nn_repulsion u
!>  Compute the nuclear-nuclear repulsion energy
!>  Returns a scalar
c----------------------------------------------------------------------------

!     The old Xijk(NT,7) array wich contains the ijk triplet information.  
      integer(C_INT), intent(in)::array_slot_0
      integer(C_INT), intent(in)::rank_0
      integer(C_INT), intent(in)::index_values_0(1:rank_0)
      integer(C_INT), intent(in)::size_0
      integer(C_INT), intent(in)::extents_0(1:rank_0)
      real(C_DOUBLE), intent(out)::data_0(1:size_0)

      integer(C_INT), intent(out)::ierr 

!     one dimensional array of atomic coordinates  
      TYPE(C_PTR) :: sDimCoords
      TYPE(C_PTR) :: coords
      integer, pointer :: pdim_coords(:)
      real(C_DOUBLE), pointer :: pcoords(:,:)

!     atomic charges     
      TYPE(C_PTR) :: charge, atoms
      integer, pointer :: patoms(:)
      real(C_DOUBLE), pointer :: pcharge(:)

      integer one, two
      integer ncenters 
      double precision energy

c-----------------------------------------------------------------------
c Get the number of atoms 
c------------------------------------------------------------------------
      ncenters = int_constant("ncenters"//C_NULL_CHAR) 
c-----------------------------------------------------------------------
c Get the nuclear coordinates and charges  
c------------------------------------------------------------------------
      call predefined_scalar_array("charge"//C_NULL_CHAR,
     *                               one, atoms ,charge)
      call c_f_pointer(atoms, patoms,[1])
      call c_f_pointer(charge, pcharge, patoms)
c ----------------------------------------------------------------------
c  ATOMIC COORDINATES  
      call predefined_scalar_array("coords"//C_NULL_CHAR,
     *                                  two, sDimCoords,  coords)
      call c_f_pointer(sDimCoords, pdim_coords,[2]) 
      call c_f_pointer(coords, pcoords, pdim_coords) 
c ----------------------------------------------------------------------

      energy = 0.0
      call return_nn_repulsion(energy, ncenters, pcoords, pcharge) 
      data_0(1) = energy

      return
      end

      subroutine return_nn_repulsion(energy, ncenters, coords, charge) 

c---------------------------------------------------------------------------

      use, intrinsic :: ISO_C_BINDING 
      implicit none 
      
c---------------------------------------------------------------------------
c Interface added but should be included instead 

      include 'sip_interface.f'

c array defining which atoms are in which fragment 
      TYPE(C_PTR) :: sDimfrag  
      TYPE(C_PTR) :: pwatom_frag 
      integer, pointer :: pdim_frag(:)  
      integer(C_INT), pointer :: watom_frag(:,:)
c array defining the number of atoms per fragment 
      TYPE(C_PTR) :: pnatoms_frag 
      integer(C_INT), pointer :: natoms_frag(:)

      integer (C_INT) nfrags

c---------------------------------------------------------------------------
      
      integer ncenters, i, j, kcoord, one, two     
      integer ifrag, jfrag, iatom, jatom
      double precision coords(3, ncenters), charge(ncenters), energy, 
     *                 dist   
      
c---------------------------------------------------------------------------
c Get predefined data 
c---------------------------------------------------------------------------

      nfrags          = int_constant("nfrags"//C_NULL_CHAR)

      if (nfrags .le. 1) then 
         energy = 0.0 
         do i = 1, ncenters 
         do j = i, ncenters 
            if (i .ne. j) then 
               dist = 0.0 
               do kcoord = 1, 3 
                  dist = dist + (coords(kcoord,i)-coords(kcoord,j))**2  
               enddo
               dist = dsqrt(dist) 
               energy = energy + charge(i)*charge(j) / dist    
            endif 
         enddo  
         enddo  
         return 
      endif 

c array defining which atoms are in which fragments 
c ----------------------------------------------------------------------
      call predefined_int_array("watom_frag"//C_NULL_CHAR,
     *                                  two, sDimfrag,  pwatom_frag)
      call c_f_pointer(sDimfrag, pdim_frag,[2])
      call c_f_pointer(pwatom_frag, watom_frag, pdim_frag)
c array defining the number of atoms in each fragment 
c ----------------------------------------------------------------------
      call predefined_int_array("natoms_frag"//C_NULL_CHAR,
     *                                  one, sDimfrag,  pnatoms_frag)
      call c_f_pointer(sDimfrag, pdim_frag,[1])
      call c_f_pointer(pnatoms_frag, natoms_frag, pdim_frag)

      if (nfrags .ge. 2) then 
         energy = 0.0 
         do ifrag = 1, nfrags 
         do jfrag = 1, nfrags 
            if (ifrag .ne. jfrag) then 
         do i = 1, natoms_frag(ifrag)  
         do j = 1, natoms_frag(jfrag)  
               dist = 0.0 
               iatom = watom_frag(ifrag,i) 
               jatom = watom_frag(jfrag,j) 
               do kcoord = 1, 3 
                  dist = dist  
     *                 + (coords(kcoord,iatom)-coords(kcoord,jatom))**2  
               enddo
               dist = dsqrt(dist) 
               energy = energy + charge(iatom)*charge(jatom) / dist    
         enddo  
         enddo  
            endif 
         enddo  
         enddo  
         energy = energy/2.0 
      endif 

      return 
      end 

