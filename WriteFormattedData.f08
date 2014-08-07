module FormattedOutput

	integer :: NumOfTable

interface WriteToFile
	module procedure WriteItems, WriteTable
end interface ! WriteToFile

contains
	subroutine WriteItems(FileUnit,Item1Name,Item1,Item2Name,Item2,Item3Name,Item3,Item4Name,Item4,Item5Name,Item5,&
&						Item6Name,Item6,Item7Name,Item7,Item8Name,Item8,Item9Name,Item9,Item10Name,Item10,Item11Name,Item11)
		
		integer :: FileUnit
		real :: Item1
		character(*), optional ::Item1Name, Item2Name,Item3Name,Item4Name,Item5Name,Item6Name,Item7Name,Item8Name,Item9Name,Item10Name,Item11Name
		real, optional :: Item2,Item3,Item4,Item5,Item6,Item7,Item8,Item9,Item10,Item11

		real, dimension(:), allocatable :: DataSet
		integer, dimension(:), allocatable :: ItemSizes, NamesSizes
		integer :: NumOfItems, TableWidth
		character(1024), dimension(:), allocatable :: ItemNames, FormatString
		character(1024), dimension(:),allocatable :: ItemFmt
		character(200) :: DataNamesString
		character(1024) :: HorisontalLinesFormat
		character(1024) :: TmpString1, TmpString2


		NumOfItems = 1
		NumOfTable = NumOfTable+1

		if (present(Item2)) then 
			NumOfItems = NumOfItems+1
		end if
		if (present(Item3)) then 
			NumOfItems = NumOfItems+1
		end if
		if (present(Item4)) then 
			NumOfItems = NumOfItems+1
		end if
		if (present(Item5)) then 
			NumOfItems = NumOfItems+1
		end if
		if (present(Item6)) then 
			NumOfItems = NumOfItems+1
		end if
		if (present(Item7)) then 
			NumOfItems = NumOfItems+1
		end if
		if (present(Item8)) then 
			NumOfItems = NumOfItems+1
		end if
		if (present(Item9)) then 
			NumOfItems = NumOfItems+1
		end if
		if (present(Item10)) then 
			NumOfItems = NumOfItems+1
		end if
		if (present(Item11)) then 
			NumOfItems = NumOfItems+1
		end if

		Allocate(DataSet(NumOfItems))
		Allocate(ItemSizes(NumOfItems))
		Allocate(ItemNames(NumOfItems))
		Allocate(NamesSizes(NumOfItems))
		Allocate(ItemFmt(NumOfItems))
		Allocate(FormatString(NumOfItems))

			DataSet(1)  = Item1
			NamesSizes(1)  = len(trim(Item1Name))
			ItemNames(1)  = Item1Name

		if (present(Item2)) then 
			DataSet(2)  = Item2
			NamesSizes(2)  = len(trim(Item2Name))
			ItemNames(2)  = Item2Name
		end if
		if (present(Item3)) then 
			DataSet(3)  = Item3
			NamesSizes(3)  = len(trim(Item3Name))
			ItemNames(3)  = Item3Name
		end if
		if (present(Item4)) then 
			DataSet(4)  = Item4
			NamesSizes(4)  = len(trim(Item4Name))
			ItemNames(4)  = Item4Name
		end if
		if (present(Item5)) then 
			DataSet(5)  = Item5
			NamesSizes(5)  = len(trim(Item5Name))
			ItemNames(5)  = Item5Name
		end if
		if (present(Item6)) then 
			DataSet(6)  = Item6
			NamesSizes(6)  = len(trim(Item6Name))
			ItemNames(6)  = Item6Name
		end if
		if (present(Item7)) then 
			DataSet(7)  = Item7
			NamesSizes(7)  = len(trim(Item7Name))
			ItemNames(7)  = Item7Name
		end if
		if (present(Item8)) then 
			DataSet(8)  = Item8
			NamesSizes(8)  = len(trim(Item8Name))
			ItemNames(8)  = Item8Name
		end if
		if (present(Item9)) then 
			DataSet(9)  = Item9
			NamesSizes(9)  = len(trim(Item9Name))
			ItemNames(9)  = Item9Name
		end if
		if (present(Item10)) then 
			DataSet(10) = Item10
			NamesSizes(10) = len(trim(Item10Name))
			ItemNames(10) = Item10Name
		end if
		if (present(Item11)) then 
			DataSet(11) = Item11
			NamesSizes(11) = len(trim(Item11Name))
			ItemNames(11) = Item11Name
		end if

		do i=1,NumOfItems
			a=int(maxval(DataSet))/10
			ItemSizes(i)=1
			do while (a>1)
				ItemSizes(i) = ItemSizes(i)+1
				a=a/10
			end do
			if (ItemSizes(i) .eq. 0) then
				ItemSizes(i) = 1
			end if
		end do

		do i=1,NumOfItems
			if (mod(DataSet(i), 1.) .eq. 0) then 
				ItemSizes(i) =ItemSizes(i)+2
				write(TmpString1,"(i5)"), ItemSizes(i)	
				ItemFmt(i) = "f" // adjustl(trim(TmpString1)) //".0"
				ItemFmt(i) = "(" // trim(ItemFmt(i)) // ")"
			else 
				ItemSizes(i) =ItemSizes(i)+9
				write(TmpString1,"(i5)"), ItemSizes(i)	
				ItemFmt(i) = "f" // adjustl(trim(TmpString1))//".6"
				ItemFmt(i) = "(" // trim(ItemFmt(i)) // ")"
			end if
		end do

		TmpString1 = ' '
		do i=1, NumOfItems
			write(TmpString2, ItemFmt(i)), DataSet(i)
			TmpString1 ="| " // trim(TmpString1) //trim(ItemNames(i)) // " = " // trim(TmpString2) //" |'"
		end do

		write(FileUnit,"(40a)"), TmpString1

		TableWidth = 0
		do i = 1, NumOfItems
			TableWidth = TableWidth + ItemSizes(i)
		end do
		TableWidth = TableWidth + NumOfItems * 4

		call WriteHorisontalLine("=", TableWidth, FileUnit)
		write(FileUnit,'( )')
	end subroutine


	subroutine WriteTable(FileUnit,TableName,Col1Name,Col1,Col2Name,Col2,Col3Name,Col3,Col4Name,Col4,Col5Name,Col5,&
&						Col6Name,Col6,Col7Name,Col7,Col8Name,Col8,Col9Name,Col9,Col10Name,Col10,Col11Name,Col11)
		
		integer :: FileUnit
		real,dimension(:) :: Col1
		character(*),optional :: Col1Name,Col2Name,Col3Name,Col4Name,Col5Name,Col6Name,Col7Name,Col8Name,Col9Name,Col10Name,Col11Name,TableName
		real,dimension(:),optional :: Col2,Col3,Col4,Col5,Col6,Col7,Col8,Col9,Col10,Col11


		real, dimension(:,:), allocatable :: DataSet
		integer, dimension(:),allocatable :: ColSizes, NamesSizes
		integer :: MaxArrSize, NumOfCols, TableWidth
		character(100), dimension(:),allocatable :: ColNames
		character, dimension(:),allocatable :: ColFmt
		character(200) :: DataNamesString
		character(1024) :: FormatString, HorisontalLinesFormat
		character(1024) :: TmpString1,TmpString2


		MaxArrSize = max(size(Col1),1)
		NumOfCols = 1
		NumOfTable = NumOfTable+1

		if (present(Col2)) then 
			NumOfCols = NumOfCols+1
			MaxArrSize = max(size(Col1),size(Col2))
		end if
		if (present(Col3)) then 
			NumOfCols = NumOfCols+1
			MaxArrSize = max(size(Col1),size(Col2),size(Col3))
		end if
		if (present(Col4)) then 
			NumOfCols = NumOfCols+1
			MaxArrSize = max(size(Col1),size(Col2),size(Col3),size(Col4))
		end if
		if (present(Col5)) then 
			NumOfCols = NumOfCols+1
			MaxArrSize = max(size(Col1),size(Col2),size(Col3),size(Col4),size(Col5))
		end if
		if (present(Col6)) then 
			NumOfCols = NumOfCols+1
			MaxArrSize = max(size(Col1),size(Col2),size(Col3),size(Col4),size(Col5),size(Col6))
		end if
		if (present(Col7)) then 
			NumOfCols = NumOfCols+1
			MaxArrSize = max(size(Col1),size(Col2),size(Col3),size(Col4),size(Col5),size(Col6),size(Col7))
		end if
		if (present(Col8)) then 
			NumOfCols = NumOfCols+1
			MaxArrSize = max(size(Col1),size(Col2),size(Col3),size(Col4),size(Col5),size(Col6),size(Col7),size(Col8))
		end if
		if (present(Col9)) then 
			NumOfCols = NumOfCols+1
			MaxArrSize = max(size(Col1),size(Col2),size(Col3),size(Col4),size(Col5),size(Col6),size(Col7),size(Col8),size(Col9))
		end if
		if (present(Col10)) then 
			NumOfCols = NumOfCols+1
			MaxArrSize = max(size(Col1),size(Col2),size(Col3),size(Col4),size(Col5),size(Col6),size(Col7),size(Col8),size(Col9),size(Col10))
		end if
		if (present(Col11)) then 
			NumOfCols = NumOfCols+1
			MaxArrSize = max(size(Col1),size(Col2),size(Col3),size(Col4),size(Col5),size(Col6),size(Col7),size(Col8),size(Col9),size(Col10),size(Col11))
		end if

		Allocate(DataSet(MaxArrSize,NumOfCols))
		Allocate(ColSizes(NumOfCols))
		Allocate(ColNames(NumOfCols))
		Allocate(NamesSizes(NumOfCols))
		Allocate(ColFmt(NumOfCols))


			DataSet(:,1)  = Col1
			NamesSizes(1)  = len(trim(Col1Name))
			ColNames(1)  = Col1Name

		if (present(Col2)) then 
			DataSet(:,2)  = Col2
			NamesSizes(2)  = len(trim(Col2Name))
			ColNames(2)  = Col2Name
		end if
		if (present(Col3)) then 
			DataSet(:,3)  = Col3
			NamesSizes(3)  = len(trim(Col3Name))
			ColNames(3)  = Col3Name
		end if
		if (present(Col4)) then 
			DataSet(:,4)  = Col4
			NamesSizes(4)  = len(trim(Col4Name))
			ColNames(4)  = Col4Name
		end if
		if (present(Col5)) then 
			DataSet(:,5)  = Col5
			NamesSizes(5)  = len(trim(Col5Name))
			ColNames(5)  = Col5Name
		end if
		if (present(Col6)) then 
			DataSet(:,6)  = Col6
			NamesSizes(6)  = len(trim(Col6Name))
			ColNames(6)  = Col6Name
		end if
		if (present(Col7)) then 
			DataSet(:,7)  = Col7
			NamesSizes(7)  = len(trim(Col7Name))
			ColNames(7)  = Col7Name
		end if
		if (present(Col8)) then 
			DataSet(:,8)  = Col8
			NamesSizes(8)  = len(trim(Col8Name))
			ColNames(8)  = Col8Name
		end if
		if (present(Col9)) then 
			DataSet(:,9)  = Col9
			NamesSizes(9)  = len(trim(Col9Name))
			ColNames(9)  = Col9Name
		end if
		if (present(Col10)) then 
			DataSet(:,10) = Col10
			NamesSizes(10) = len(trim(Col10Name))
			ColNames(10) = Col10Name
		end if
		if (present(Col11)) then 
			DataSet(:,11) = Col11
			NamesSizes(11) = len(trim(Col11Name))
			ColNames(11) = Col11Name
		end if


		do i=1,NumOfCols
			a=int(maxval(DataSet(:,i)))/10
			ColSizes(i)=1
			do while (a>1)
				ColSizes(i) = ColSizes(i)+1
				a=a/10
			end do
			if (ColSizes(i) .eq. 0) then
				ColSizes(i) = 1
			end if
		end do


		do i=1,NumOfCols
			do j=1,MaxArrSize
				if (mod(DataSet(j,i),1.) .eq. 0) then 
					ColFmt(i) = 'i'
				else 
					ColFmt(i) = 'f'
					exit
				end if
			end do
		end do

		FormatString = '('
		do i=1,NumOfCols
			select case (ColFmt(i))
			case('i')
				ColSizes(i) = max(ColSizes(i)+3,NamesSizes(i)+2)
				write(TmpString1,"(i5)"), ColSizes(i)
				FormatString =trim(FormatString)//"'|'" // "f" // adjustl(trim(TmpString1))//".0"//"' |'"
			case('f')
				ColSizes(i) = max(ColSizes(i)+10,NamesSizes(i)+2)
				write(TmpString1,"(i5)"), ColSizes(i)
				FormatString =trim(FormatString)//"'|'" // "f" // adjustl(trim(TmpString1))//".6"//"' |'"
			end select
		end do
		FormatString = trim(FormatString) // ')'

		DataNamesString = '|'
		do i=1,NumOfCols
			write(TmpString1,"(i5)"),ColSizes(i)
			write(TmpString2,"(a10)"),"(a"//adjustl(trim(TmpString1))//")"
			write(TmpString1,trim(TmpString2)),trim(ColNames(i))
			DataNamesString = trim(DataNamesString) // trim(TmpString1)//" |'| "
		end do
		DataNamesString = DataNamesString(:len(trim(DataNamesString))-2)

		TableWidth = 0
		do i = 1, NumOfCols
			TableWidth = TableWidth + ColSizes(i)
		end do
		TableWidth = TableWidth + NumOfCols * 4

		write(TmpString1,"(i5)"),NumOfTable
		call WriteHorisontalLine("=", TableWidth, FileUnit)
		write(FileUnit,"(20x100a)"),"Tablica N. "//adjustl(trim(TmpString1))

		do i = 2, len(trim(TableName))
			if (TableName(1:3) /= "---") then
				if (TableName(i-1:i) .eq. "\n") then
					write(TmpString1, "(15x100a)"), TableName(1:i-2)
					write(TmpString2, "(15x100a)"), TableName(i+1: len(trim(TableName)))
				end if
			end if
		end do

		write(FileUnit, "(15x100a)"),adjustl(trim(TmpString1))
		write(FileUnit, "(15x100a)"), adjustl(trim(TmpString2))

		call WriteHorisontalLine("_", TableWidth, FileUnit)
		write(FileUnit,"(40a)"), trim(DataNamesString)
		call WriteHorisontalLine("_", TableWidth, FileUnit)

		do i=1,MaxArrSize
			write(FileUnit,FormatString), DataSet(i,:)
		end do
		call WriteHorisontalLine("=", TableWidth, FileUnit)
		write(FileUnit,'( )')
	end subroutine



	subroutine WriteHorisontalLine(InChar, TableWidth, FileUnit)
		character:: InChar
		integer :: TableWidth, FileUnit

		character(len=1024) :: TmpStr1
		character(len=1024) :: TmpStr2
		
		write(TmpStr1, "(i5)"), TableWidth
		TmpStr2 = "(" // trim(TmpStr1) // "('" // InChar // "')" // ")"
		write(FileUnit, TmpStr2)

	end subroutine WriteHorisontalLine

end module