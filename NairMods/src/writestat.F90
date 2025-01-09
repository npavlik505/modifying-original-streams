subroutine writestat2d
!
! Writing stat.bin
!
 use mod_streams
 implicit none
!
 integer :: l,m
 integer :: mpi_io_file
 integer :: filetype
 integer :: local_datatype
 integer, dimension(3) :: sizes     ! Dimensions of the total grid
 integer, dimension(3) :: subsizes  ! Dimensions of grid local to a procs
 integer, dimension(3) :: starts    ! Starting coordinates
 integer, dimension(3) :: memsizes
 integer :: size_real
 integer :: ntotxy
 integer (kind=mpi_offset_kind) :: offset
 integer(kind=mpi_offset_kind) :: filesize
 integer :: FiftyKrestart ! Quick fix to produce stat_ALL.bin when restarting at 50000 iterations and istat = 5
 character(8) :: nastat
!
 if (masterproc) write(*,*) 'Writing stat.bin'
 write(nastat, '(I8.8)') icyc
 1004 format(I4.4) 
!
 if (ncoords(3)==0) then
!
  sizes(1) = nblocks(1)*nx
  sizes(2) = nblocks(2)*ny
  sizes(3) = 1
  subsizes(1) = nx
  subsizes(2) = ny
  subsizes(3) = 1 
  starts(1) = 0 + ncoords(1)*subsizes(1)
  starts(2) = 0 + ncoords(2)*subsizes(2)
  starts(3) = 0
  ntotxy = nx*ny
!
  FiftyKrestart = 50005
  call mpi_type_create_subarray(3,sizes,subsizes,starts,mpi_order_fortran,mpi_prec,filetype,iermpi)
  call mpi_type_commit(filetype,iermpi)
  if (icyc == FiftyKrestart) then
   call mpi_file_open(mp_cartx,'stat_ALL.bin',mpi_mode_create+mpi_mode_wronly,mpi_info_null,mpi_io_file,iermpi)
   offset = 0
  else
   call mpi_file_open(mp_cartx, 'stat_ALL.bin', mpi_mode_wronly, mpi_info_null, mpi_io_file, iermpi)
   call mpi_file_get_size(mpi_io_file, filesize, iermpi)  ! Get current file size
   offset = filesize  ! Set offset to end of file for appending
  end if

  do l=1,nvmean
   call mpi_file_set_view(mpi_io_file,offset,mpi_prec,filetype,"native",mpi_info_null,iermpi)
   call mpi_file_write_all(mpi_io_file,w_av(l,1:nx,1:ny),ntotxy,mpi_prec,istatus,iermpi)
   call mpi_type_size(mpi_prec,size_real,iermpi)
   do m=1,nblocks(1)*nblocks(2)
    offset = offset+size_real*ntotxy
   enddo
  enddo
!
  call mpi_file_close(mpi_io_file,iermpi)
  call mpi_type_free(filetype,iermpi)
!
 endif
!
end subroutine writestat2d
!
subroutine writestat1d
!
! Writing stat.bin
!
 use mod_streams
 implicit none
!
 if (masterproc) then
!
  write(*,*)  'Writing stat.bin'
!
  open(12,file='stat.bin',form='unformatted')
  write(12) w_av_1d
  close(12)
!
 endif
!
end subroutine writestat1d
!
subroutine writestat2d_serial
!
! Writing stat file
!
 use mod_streams
 implicit none
!
 if (ncoords(3)==0) then
!
  open(unit=12,file='wavplot_'//chx//'_'//chy//'.bin',form='unformatted')
  write(12) w_av
  close(12)
!
 endif
!
end subroutine writestat2d_serial
