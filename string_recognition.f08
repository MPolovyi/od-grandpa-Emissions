module String_recognition

integer NoC
!interface RealArrayRead
!	subroutine RealArrayOneDimRead(InString,OutArray)
!		character(*) :: InString
!		real :: OutArray(:)
!
!	end subroutine
!
!	subroutine RealArrayTwoDimRead(InString,OutArray)
!		character(*) :: InString
!		real :: OutArray(:,:)
!	end subroutine
!end interface


! Take string literal and variable of some type (int, real, or array), and returns recognised variable
interface ProcessString
	module procedure IntRead,RealRead,IntArrayOneDimRead,IntArrayTwoDimRead,RealArrayOneDimRead,RealArrayTwoDimRead
end interface

contains

!Left for documentation, explains how ProceedString interface work
!subroutine ProcessString(InString,OutInt,OutReal,OutIntArr,OutRealArr,OutIntDoubleArr,OutRealDoubleArr)
!	character(*) :: InString
!	integer, optional :: OutInt, OutIntArr(:), OutIntDoubleArr(:,:)
!	real, optional :: OutReal, OutRealArr(:), OutRealDoubleArr(:,:)
!
!	if (present(OutInt)) then
!		call IntRead(InString,OutInt)
!	else if (present(OutReal)) then
!		call RealRead(InString,OutReal)
!	else if (present(OutIntArr)) then
!		call IntArrayOneDimRead(InString,OutIntArr)
!	else if (present(OutIntDoubleArr)) then
!		call IntArrayTwoDimRead(InString,OutIntDoubleArr)
!	else if (present(OutRealArr)) then
!		call RealArrayOneDimRead(InString,OutRealArr)
!	else if (present(OutRealDoubleArr)) then
!		call RealArrayTwoDimRead(InString,OutRealDoubleArr)
!	end if
!end subroutine ProceedString


subroutine IntRead(InString,OutInt)
	character(*) :: InString
	integer OutInt


	Read(InString,*), OutInt
end subroutine 


subroutine RealRead(InString,OutReal)
	character(*) :: InString

	Read(InString,*), OutReal
end subroutine 


subroutine IntArrayOneDimRead(InString,OutArray)
	character(*) :: InString
	integer,allocatable :: OutArray(:)
	integer :: NumOfItems

	NumOfItems=GetNumOfElements(InString)
	allocate(OutArray(NumOfItems))

	Read(InString,*),(OutArray(j),j=1,Size(OutArray,1))
end subroutine


subroutine RealArrayOneDimRead(InString,OutArray)
	character(*) :: InString
	real,allocatable :: OutArray(:)
	integer :: NumOfItems

	NumOfItems=GetNumOfElements(InString)
	allocate(OutArray(NumOfItems))
	Read(InString,*),(OutArray(j),j=1,Size(OutArray,1))
end subroutine


subroutine IntArrayTwoDimRead(InString,OutArray)
	character(*) :: InString
	integer :: OutArray(:,:)

	integer, dimension(:), ALLOCATABLE :: TempArray
	integer :: NumOfItems

	NumOfItems=GetNumOfElements(InString)
	Allocate(TempArray(NumOfItems))
	Read(InString,*), (TempArray(i),i=1,Size(TempArray,1))
	OutArray = transpose(reshape(TempArray,Shape(OutArray)))
	Deallocate(TempArray)
end subroutine 


subroutine RealArrayTwoDimRead(InString,OutArray)
	character(*) :: InString
	real :: OutArray(:,:)

	real,dimension(:), ALLOCATABLE :: TempArray
	integer :: NumOfItems

	NumOfItems=GetNumOfElements(InString)
	Allocate(TempArray(NumOfItems))
	Read(InString,*), (TempArray(i),i=1,Size(TempArray,1))
	OutArray = transpose(reshape(TempArray,Shape(OutArray)))
	Deallocate(TempArray)
end subroutine 


integer function GetNumOfElements(InString)
	character(*) :: InString
	integer :: NumOfItems

	NumOfItems=1
	do iterator=1,len(trim(InString))
		if (InString(iterator:iterator) .eq. ' ') then
			NumOfItems=NumOfItems+1
		end if
	end do

	GetNumOfElements=NumOfItems
end function

END module String_recognition