	module FileReading
	use String_recognition

	contains

	subroutine SplitFileToStrings(InFile,OutStrings)

      	character(*), intent(in) :: InFile
            character(*),dimension(:),allocatable :: OutStrings

            integer,parameter :: MaxStringsInFile=100

            character(500),dimension(MaxStringsInFile) :: ReadedStrings
            integer :: StringsInFile, NotCommented,Commented

            open(unit=100,file=InFile,status='old',form='formatted')
            
            StringsInFile=0
            NotCommented=0
            Commented=0
            do iterator=1,MaxStringsInFile
                  read (100,'(a500)',end=123) ReadedStrings(iterator)
                  StringsInFile = StringsInFile+1
            end do
 123  continue

            call PrepareDoubleArrays(ReadedStrings,StringsInFile)
            call CommentEmptyStrings(ReadedStrings,StringsInFile)

            do iterator=1,StringsInFile
                  if (ReadedStrings(iterator)(1:3) /= '---')then 
                        NotCommented = NotCommented+1
                  else
                        Commented=Commented+1
                  end if
            end do

            allocate(OutStrings(NotCommented))

            NotCommented=0
            Commented=0

            do iterator=1,StringsInFile
                  if (ReadedStrings(iterator)(1:3) /= '---')then 
                        OutStrings(iterator - Commented) = ReadedStrings(iterator)
                        NotCommented = NotCommented+1
                  else
                        Commented=Commented+1
                  end if
            end do

            do i=1,NotCommented
                  call RemoveDoubleSpaces(OutStrings(i))
            end do
      end subroutine

      subroutine PrepareDoubleArrays(InStrings, NumOfStrings)
            character(*), dimension(:) :: InStrings
            integer :: NumOfStrings

            integer :: StrLen
            character :: TestChar

            do iterator=1,NumOfStrings
                  StrLen = len(trim(InStrings(iterator)))
                  TestChar = InStrings(iterator)(StrLen:StrLen)
                  if (TestChar .eq. '+' ) then 
                        InStrings(iterator+1) = InStrings(iterator)(:StrLen-1) // ' ' //
     >                   InStrings(iterator+1)(:len(trim(InStrings(iterator+1))))
                        InStrings(iterator) = '---'
                  end if
            end do
      end subroutine

      subroutine CommentEmptyStrings(InStrings, NumOfStrings)
            character(*), dimension(:) :: InStrings
            integer :: NumOfStrings

            do iterator=1,NumOfStrings
                  if (len(trim(InStrings(iterator))) .eq. 0) then
                        InStrings(iterator) = '---'
                  end if
            end do
      end subroutine

      subroutine RemoveDoubleSpaces(InString)
            character(*) :: InString
            integer :: StrLen1, StrLen2

            StrLen1=0
            StrLen2=len(trim(InString))

            do j=1,10
                  if (StrLen1 /= StrLen2) then
                        do i=1,len(trim(InString))
                              if (InString(i:i+1) .eq. '  ') then
                                    InString=InString(:i)//InString(i+2:)
                              end if
                        end do
                        StrLen2=StrLen1
                        StrLen1=len(trim(InString))
                  end if
            end do
      end subroutine

c first we read integers, after - reals, IntArrays, IntDoubleDimArrays, RealArrays, RealDoubleDimArrays
      subroutine ReadDataFromFile(InFile,
     >Int1,Int2,Int3,Int4,Int5,Int6,Int7,Int8,Int9,Int10,Int11,Int12,Int13,Int14,Int15,Int16,
     >Int17,Int18,Int19,Int20,Int21,Int22,Int23,Int24,Int25,Int26,Int27,Int28,Int29,Int30,Int31,
     >Int32,Int33,Int34,Int35,Int36,Int37,Int38,Int39,Real1,Real2,Real3,Real4,Real5,Real6,
     >Real7,Real8,Real9,Real10,Real11,Real12,Real13,Real14,Real15,Real16,Real17,Real18,Real19,
     >Real20,Real21,Real22,Real23,Real24,Real25,Real26,Real27,Real28,Real29,Real30,Real31,Real32,
     >Real33,Real34,Real35,Real36,Real37,Real38,Real39,IntArrOne1,IntArrOne2,IntArrOne3,
     >IntArrOne4,IntArrOne5,IntArrOne6,IntArrOne7,IntArrOne8,IntArrOne9,IntArrOne10,IntArrOne11,
     >IntArrOne12,IntArrOne13,IntArrOne14,IntArrOne15,IntArrOne16,IntArrOne17,IntArrOne18,IntArrOne19,
     >IntArrOne20,IntArrOne21,IntArrOne22,IntArrOne23,IntArrOne24,IntArrOne25,IntArrOne26,IntArrOne27,
     >IntArrOne28,IntArrOne29,IntArrOne30,IntArrOne31,IntArrOne32,IntArrOne33,IntArrOne34,IntArrOne35,
     >IntArrOne36,IntArrOne37,IntArrOne38,IntArrOne39,IntArrTwo1,IntArrTwo2,IntArrTwo3,
     >IntArrTwo4,IntArrTwo5,IntArrTwo6,IntArrTwo7,IntArrTwo8,IntArrTwo9,IntArrTwo10,IntArrTwo11,
     >IntArrTwo12,IntArrTwo13,IntArrTwo14,IntArrTwo15,IntArrTwo16,IntArrTwo17,IntArrTwo18,IntArrTwo19,
     >IntArrTwo20,IntArrTwo21,IntArrTwo22,IntArrTwo23,IntArrTwo24,IntArrTwo25,IntArrTwo26,IntArrTwo27,
     >IntArrTwo28,IntArrTwo29,IntArrTwo30,IntArrTwo31,IntArrTwo32,IntArrTwo33,IntArrTwo34,IntArrTwo35,
     >IntArrTwo36,IntArrTwo37,IntArrTwo38,IntArrTwo39,RealArrOne1,RealArrOne2,RealArrOne3,
     >RealArrOne4,RealArrOne5,RealArrOne6,RealArrOne7,RealArrOne8,RealArrOne9,RealArrOne10,RealArrOne11,
     >RealArrOne12,RealArrOne13,RealArrOne14,RealArrOne15,RealArrOne16,RealArrOne17,RealArrOne18,RealArrOne19,
     >RealArrOne20,RealArrOne21,RealArrOne22,RealArrOne23,RealArrOne24,RealArrOne25,RealArrOne26,RealArrOne27,
     >RealArrOne28,RealArrOne29,RealArrOne30,RealArrOne31,RealArrOne32,RealArrOne33,RealArrOne34,RealArrOne35,
     >RealArrOne36,RealArrOne37,RealArrOne38,RealArrOne39,RealArrTwo1,RealArrTwo2,RealArrTwo3,
     >RealArrTwo4,RealArrTwo5,RealArrTwo6,RealArrTwo7,RealArrTwo8,RealArrTwo9,RealArrTwo10,RealArrTwo11,
     >RealArrTwo12,RealArrTwo13,RealArrTwo14,RealArrTwo15,RealArrTwo16,RealArrTwo17,RealArrTwo18,RealArrTwo19,
     >RealArrTwo20,RealArrTwo21,RealArrTwo22,RealArrTwo23,RealArrTwo24,RealArrTwo25,RealArrTwo26,RealArrTwo27,
     >RealArrTwo28,RealArrTwo29,RealArrTwo30,RealArrTwo31,RealArrTwo32,RealArrTwo33,RealArrTwo34,RealArrTwo35,
     >RealArrTwo36,RealArrTwo37,RealArrTwo38,RealArrTwo39)

      integer, optional :: Int1,Int2,Int3,Int4,Int5,Int6,Int7,Int8,Int9,Int10,Int11,Int12,Int13,Int14,Int15,Int16,
     >Int17,Int18,Int19,Int20,Int21,Int22,Int23,Int24,Int25,Int26,Int27,Int28,Int29,Int30,Int31,
     >Int32,Int33,Int34,Int35,Int36,Int37,Int38,Int39

      real, optional :: Real1,Real2,Real3,Real4,Real5,Real6,Real7,Real8,Real9,Real10,Real11,Real12,Real13,Real14,
     >Real15,Real16,Real17,Real18,Real19,
     >Real20,Real21,Real22,Real23,Real24,Real25,Real26,Real27,Real28,Real29,Real30,Real31,Real32,
     >Real33,Real34,Real35,Real36,Real37,Real38,Real39

      integer,dimension(:),allocatable, optional :: IntArrOne1,IntArrOne2,IntArrOne3,
     >IntArrOne4,IntArrOne5,IntArrOne6,IntArrOne7,IntArrOne8,IntArrOne9,IntArrOne10,IntArrOne11,
     >IntArrOne12,IntArrOne13,IntArrOne14,IntArrOne15,IntArrOne16,IntArrOne17,IntArrOne18,IntArrOne19,
     >IntArrOne20,IntArrOne21,IntArrOne22,IntArrOne23,IntArrOne24,IntArrOne25,IntArrOne26,IntArrOne27,
     >IntArrOne28,IntArrOne29,IntArrOne30,IntArrOne31,IntArrOne32,IntArrOne33,IntArrOne34,IntArrOne35,
     >IntArrOne36,IntArrOne37,IntArrOne38,IntArrOne39

      integer,dimension(:,:), optional :: IntArrTwo1,IntArrTwo2,IntArrTwo3,
     >IntArrTwo4,IntArrTwo5,IntArrTwo6,IntArrTwo7,IntArrTwo8,IntArrTwo9,IntArrTwo10,IntArrTwo11,
     >IntArrTwo12,IntArrTwo13,IntArrTwo14,IntArrTwo15,IntArrTwo16,IntArrTwo17,IntArrTwo18,IntArrTwo19,
     >IntArrTwo20,IntArrTwo21,IntArrTwo22,IntArrTwo23,IntArrTwo24,IntArrTwo25,IntArrTwo26,IntArrTwo27,
     >IntArrTwo28,IntArrTwo29,IntArrTwo30,IntArrTwo31,IntArrTwo32,IntArrTwo33,IntArrTwo34,IntArrTwo35,
     >IntArrTwo36,IntArrTwo37,IntArrTwo38,IntArrTwo39

      real, dimension(:),allocatable, optional :: RealArrOne1,RealArrOne2,RealArrOne3,
     >RealArrOne4,RealArrOne5,RealArrOne6,RealArrOne7,RealArrOne8,RealArrOne9,RealArrOne10,RealArrOne11,
     >RealArrOne12,RealArrOne13,RealArrOne14,RealArrOne15,RealArrOne16,RealArrOne17,RealArrOne18,RealArrOne19,
     >RealArrOne20,RealArrOne21,RealArrOne22,RealArrOne23,RealArrOne24,RealArrOne25,RealArrOne26,RealArrOne27,
     >RealArrOne28,RealArrOne29,RealArrOne30,RealArrOne31,RealArrOne32,RealArrOne33,RealArrOne34,RealArrOne35,
     >RealArrOne36,RealArrOne37,RealArrOne38,RealArrOne39

      real, dimension(:,:), optional :: RealArrTwo1,RealArrTwo2,RealArrTwo3,
     >RealArrTwo4,RealArrTwo5,RealArrTwo6,RealArrTwo7,RealArrTwo8,RealArrTwo9,RealArrTwo10,RealArrTwo11,
     >RealArrTwo12,RealArrTwo13,RealArrTwo14,RealArrTwo15,RealArrTwo16,RealArrTwo17,RealArrTwo18,RealArrTwo19,
     >RealArrTwo20,RealArrTwo21,RealArrTwo22,RealArrTwo23,RealArrTwo24,RealArrTwo25,RealArrTwo26,RealArrTwo27,
     >RealArrTwo28,RealArrTwo29,RealArrTwo30,RealArrTwo31,RealArrTwo32,RealArrTwo33,RealArrTwo34,RealArrTwo35,
     >RealArrTwo36,RealArrTwo37,RealArrTwo38,RealArrTwo39

      character(*) :: InFile
            integer :: iterator
            character(500),dimension(:),allocatable :: DataStrings

            call SplitFileToStrings(InFile,DataStrings)

      iterator=0

      if (present(Int1)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int1)
      end if
       if (present(Int2)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int2)
      end if
       if (present(Int3)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int3)
      end if
       if (present(Int4)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int4)
      end if
       if (present(Int5)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int5)
      end if
       if (present(Int6)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int6)
      end if
       if (present(Int7)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int7)
      end if
       if (present(Int8)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int8)
      end if
       if (present(Int9)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int9)
      end if
       if (present(Int10)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int10)
      end if
       if (present(Int11)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int11)
      end if
       if (present(Int12)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int12)
      end if
       if (present(Int13)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int13)
      end if
       if (present(Int14)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int14)
      end if
       if (present(Int15)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int15)
      end if
       if (present(Int16)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int16)
      end if
       if (present(Int17)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int17)
      end if
       if (present(Int18)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int18)
      end if
       if (present(Int19)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int19)
      end if
       if (present(Int20)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int20)
      end if
       if (present(Int21)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int21)
      end if
       if (present(Int22)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int22)
      end if
       if (present(Int23)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int23)
      end if
       if (present(Int24)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int24)
      end if
       if (present(Int25)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int25)
      end if
       if (present(Int26)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int26)
      end if
       if (present(Int27)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int27)
      end if
       if (present(Int28)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int28)
      end if
       if (present(Int29)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int29)
      end if
       if (present(Int30)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int30)
      end if
       if (present(Int31)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int31)
      end if
       if (present(Int32)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int32)
      end if
       if (present(Int33)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int33)
      end if
       if (present(Int34)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int34)
      end if
       if (present(Int35)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int35)
      end if
       if (present(Int36)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int36)
      end if
       if (present(Int37)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int37)
      end if
       if (present(Int37)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int37)
      end if
       if (present(Int38)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int38)
      end if
       if (present(Int39)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Int39)


      end if
       if (present(Real1)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real1)
      end if
       if (present(Real2)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real2)
      end if
       if (present(Real3)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real3)
      end if
       if (present(Real4)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real4)
      end if
       if (present(Real5)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real5)
      end if
       if (present(Real6)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real6)
      end if
       if (present(Real7)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real7)
      end if
       if (present(Real8)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real8)
      end if
       if (present(Real9)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real9)
      end if
       if (present(Real10)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real10)
      end if
       if (present(Real11)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real11)
      end if
       if (present(Real12)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real12)
      end if
       if (present(Real13)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real13)
      end if
       if (present(Real14)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real14)
      end if
       if (present(Real15)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real15)
      end if
       if (present(Real16)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real16)
      end if
       if (present(Real17)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real17)
      end if
       if (present(Real18)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real18)
      end if
       if (present(Real19)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real19)
      end if
       if (present(Real20)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real20)
      end if
       if (present(Real21)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real21)
      end if
       if (present(Real22)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real22)
      end if
       if (present(Real23)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real23)
      end if
       if (present(Real24)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real24)
      end if
       if (present(Real25)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real25)
      end if
       if (present(Real26)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real26)
      end if
       if (present(Real27)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real27)
      end if
       if (present(Real28)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real28)
      end if
       if (present(Real29)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real29)
      end if
       if (present(Real30)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real30)
      end if
       if (present(Real31)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real31)
      end if
       if (present(Real32)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real32)
      end if
       if (present(Real33)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real33)
      end if
       if (present(Real34)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real34)
      end if
       if (present(Real35)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real35)
      end if
       if (present(Real36)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real36)
      end if
       if (present(Real37)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real37)
      end if
       if (present(Real37)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real37)
      end if
       if (present(Real38)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real38)
      end if
       if (present(Real39)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),Real39)


      end if
       if (present(IntArrOne1)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne1)
      end if
       if (present(IntArrOne2)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne2)
      end if
       if (present(IntArrOne3)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne3)
      end if
       if (present(IntArrOne4)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne4)
      end if
       if (present(IntArrOne5)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne5)
      end if
       if (present(IntArrOne6)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne6)
      end if
       if (present(IntArrOne7)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne7)
      end if
       if (present(IntArrOne8)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne8)
      end if
       if (present(IntArrOne9)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne9)
      end if
       if (present(IntArrOne10)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne10)
      end if
       if (present(IntArrOne11)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne11)
      end if
       if (present(IntArrOne12)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne12)
      end if
       if (present(IntArrOne13)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne13)
      end if
       if (present(IntArrOne14)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne14)
      end if
       if (present(IntArrOne15)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne15)
      end if
       if (present(IntArrOne16)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne16)
      end if
       if (present(IntArrOne17)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne17)
      end if
       if (present(IntArrOne18)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne18)
      end if
       if (present(IntArrOne19)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne19)
      end if
       if (present(IntArrOne20)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne20)
      end if
       if (present(IntArrOne21)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne21)
      end if
       if (present(IntArrOne22)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne22)
      end if
       if (present(IntArrOne23)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne23)
      end if
       if (present(IntArrOne24)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne24)
      end if
       if (present(IntArrOne25)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne25)
      end if
       if (present(IntArrOne26)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne26)
      end if
       if (present(IntArrOne27)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne27)
      end if
       if (present(IntArrOne28)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne28)
      end if
       if (present(IntArrOne29)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne29)
      end if
       if (present(IntArrOne30)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne30)
      end if
       if (present(IntArrOne31)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne31)
      end if
       if (present(IntArrOne32)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne32)
      end if
       if (present(IntArrOne33)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne33)
      end if
       if (present(IntArrOne34)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne34)
      end if
       if (present(IntArrOne35)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne35)
      end if
       if (present(IntArrOne36)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne36)
      end if
       if (present(IntArrOne37)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne37)
      end if
       if (present(IntArrOne37)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne37)
      end if
       if (present(IntArrOne38)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne38)
      end if
       if (present(IntArrOne39)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrOne39)


      end if
       if (present(IntArrTwo1)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo1)
      end if
       if (present(IntArrTwo2)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo2)
      end if
       if (present(IntArrTwo3)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo3)
      end if
       if (present(IntArrTwo4)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo4)
      end if
       if (present(IntArrTwo5)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo5)
      end if
       if (present(IntArrTwo6)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo6)
      end if
       if (present(IntArrTwo7)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo7)
      end if
       if (present(IntArrTwo8)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo8)
      end if
       if (present(IntArrTwo9)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo9)
      end if
       if (present(IntArrTwo10)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo10)
      end if
       if (present(IntArrTwo11)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo11)
      end if
       if (present(IntArrTwo12)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo12)
      end if
       if (present(IntArrTwo13)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo13)
      end if
       if (present(IntArrTwo14)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo14)
      end if
       if (present(IntArrTwo15)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo15)
      end if
       if (present(IntArrTwo16)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo16)
      end if
       if (present(IntArrTwo17)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo17)
      end if
       if (present(IntArrTwo18)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo18)
      end if
       if (present(IntArrTwo19)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo19)
      end if
       if (present(IntArrTwo20)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo20)
      end if
       if (present(IntArrTwo21)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo21)
      end if
       if (present(IntArrTwo22)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo22)
      end if
       if (present(IntArrTwo23)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo23)
      end if
       if (present(IntArrTwo24)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo24)
      end if
       if (present(IntArrTwo25)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo25)
      end if
       if (present(IntArrTwo26)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo26)
      end if
       if (present(IntArrTwo27)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo27)
      end if
       if (present(IntArrTwo28)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo28)
      end if
       if (present(IntArrTwo29)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo29)
      end if
       if (present(IntArrTwo30)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo30)
      end if
       if (present(IntArrTwo31)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo31)
      end if
       if (present(IntArrTwo32)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo32)
      end if
       if (present(IntArrTwo33)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo33)
      end if
       if (present(IntArrTwo34)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo34)
      end if
       if (present(IntArrTwo35)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo35)
      end if
       if (present(IntArrTwo36)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo36)
      end if
       if (present(IntArrTwo37)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo37)
      end if
       if (present(IntArrTwo37)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo37)
      end if
       if (present(IntArrTwo38)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo38)
      end if
       if (present(IntArrTwo39)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),IntArrTwo39)


      end if
       if (present(RealArrOne1)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne1)
      end if
       if (present(RealArrOne2)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne2)
      end if
       if (present(RealArrOne3)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne3)
      end if
       if (present(RealArrOne4)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne4)
      end if
       if (present(RealArrOne5)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne5)
      end if
       if (present(RealArrOne6)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne6)
      end if
       if (present(RealArrOne7)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne7)
      end if
       if (present(RealArrOne8)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne8)
      end if
       if (present(RealArrOne9)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne9)
      end if
       if (present(RealArrOne10)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne10)
      end if
       if (present(RealArrOne11)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne11)
      end if
       if (present(RealArrOne12)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne12)
      end if
       if (present(RealArrOne13)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne13)
      end if
       if (present(RealArrOne14)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne14)
      end if
       if (present(RealArrOne15)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne15)
      end if
       if (present(RealArrOne16)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne16)
      end if
       if (present(RealArrOne17)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne17)
      end if
       if (present(RealArrOne18)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne18)
      end if
       if (present(RealArrOne19)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne19)
      end if
       if (present(RealArrOne20)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne20)
      end if
       if (present(RealArrOne21)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne21)
      end if
       if (present(RealArrOne22)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne22)
      end if
       if (present(RealArrOne23)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne23)
      end if
       if (present(RealArrOne24)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne24)
      end if
       if (present(RealArrOne25)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne25)
      end if
       if (present(RealArrOne26)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne26)
      end if
       if (present(RealArrOne27)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne27)
      end if
       if (present(RealArrOne28)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne28)
      end if
       if (present(RealArrOne29)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne29)
      end if
       if (present(RealArrOne30)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne30)
      end if
       if (present(RealArrOne31)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne31)
      end if
       if (present(RealArrOne32)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne32)
      end if
       if (present(RealArrOne33)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne33)
      end if
       if (present(RealArrOne34)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne34)
      end if
       if (present(RealArrOne35)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne35)
      end if
       if (present(RealArrOne36)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne36)
      end if
       if (present(RealArrOne37)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne37)
      end if
       if (present(RealArrOne37)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne37)
      end if
       if (present(RealArrOne38)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne38)
      end if
       if (present(RealArrOne39)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrOne39)
      end if


       if (present(RealArrTwo1)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo1)
      end if
       if (present(RealArrTwo2)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo2)
      end if
       if (present(RealArrTwo3)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo3)
      end if
       if (present(RealArrTwo4)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo4)
      end if
       if (present(RealArrTwo5)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo5)
      end if
       if (present(RealArrTwo6)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo6)
      end if
       if (present(RealArrTwo7)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo7)
      end if
       if (present(RealArrTwo8)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo8)
      end if
       if (present(RealArrTwo9)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo9)
      end if
       if (present(RealArrTwo10)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo10)
      end if
       if (present(RealArrTwo11)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo11)
      end if
       if (present(RealArrTwo12)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo12)
      end if
       if (present(RealArrTwo13)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo13)
      end if
       if (present(RealArrTwo14)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo14)
      end if
       if (present(RealArrTwo15)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo15)
      end if
       if (present(RealArrTwo16)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo16)
      end if
       if (present(RealArrTwo17)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo17)
      end if
       if (present(RealArrTwo18)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo18)
      end if
       if (present(RealArrTwo19)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo19)
      end if
       if (present(RealArrTwo20)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo20)
      end if
       if (present(RealArrTwo21)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo21)
      end if
       if (present(RealArrTwo22)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo22)
      end if
       if (present(RealArrTwo23)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo23)
      end if
       if (present(RealArrTwo24)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo24)
      end if
       if (present(RealArrTwo25)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo25)
      end if
       if (present(RealArrTwo26)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo26)
      end if
       if (present(RealArrTwo27)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo27)
      end if
       if (present(RealArrTwo28)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo28)
      end if
       if (present(RealArrTwo29)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo29)
      end if
       if (present(RealArrTwo30)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo30)
      end if
       if (present(RealArrTwo31)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo31)
      end if
       if (present(RealArrTwo32)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo32)
      end if
       if (present(RealArrTwo33)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo33)
      end if
       if (present(RealArrTwo34)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo34)
      end if
       if (present(RealArrTwo35)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo35)
      end if
       if (present(RealArrTwo36)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo36)
      end if
       if (present(RealArrTwo37)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo37)
      end if
       if (present(RealArrTwo37)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo37)
      end if
       if (present(RealArrTwo38)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo38)
      end if
       if (present(RealArrTwo39)) then
            iterator=iterator+1
            call ProcessString(DataStrings(iterator),RealArrTwo39)
      end if

      end subroutine
      end module

























































































