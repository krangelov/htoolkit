import Foreign
import Data.List

main = do
  let ls1 = ["/* File generated automatically; do not edit! */"
            ,"/* This file can be copied, modified and distributed without restriction. */"
            ,""
            ,""
            ,"\t.section \".init\",\"ax\""
            ,"\tjmp 1f"
            ,"__wine_spec_pe_header:"
            ,"\t.skip 69632"
            ,"1:"
            ,""
            ,"\t.data"
            ,"\t.align 8"
            ,"\t.globl __wine_spec_nt_header"
            ,"\t.hidden __wine_spec_nt_header"
            ,"__wine_spec_nt_header:"
            ,".L__wine_spec_rva_base:"
            ,"\t.long 0x4550"
            ,"\t.short 0x8664"
            ,"\t.short 0"
            ,"\t.long 0"
            ,"\t.long 0"
            ,"\t.long 0"
            ,"\t.short 240"
            ,"\t.short 0x0022"
            ,"\t.short 0x020b"
            ,"\t.byte 0"
            ,"\t.byte 0"
            ,"\t.long 0"
            ,"\t.long 0"
            ,"\t.long 0"
            ,"\t.quad __wine_spec_exe_entry"
            ,"\t.quad __wine_spec_pe_header"
            ,"\t.long 4096"
            ,"\t.long 4096"
            ,"\t.short 1,0"
            ,"\t.short 0,0"
            ,"\t.short 4,0"
            ,"\t.long 0"
            ,"\t.long _end-.L__wine_spec_rva_base"
            ,"\t.long 4096"
            ,"\t.long 0"
            ,"\t.short 0x0002"
            ,"\t.short 0x0100"
            ,"\t.quad 1048576,4096"
            ,"\t.quad 1048576,4096"
            ,"\t.long 0"
            ,"\t.long 16"
            ,"\t.long 0,0"
            ,"\t.long .L__wine_spec_imports-.L__wine_spec_rva_base,.L__wine_spec_imports_end-.L__wine_spec_imports"
            ,"\t.long 0,0"
            ,"\t.long 0,0"
            ,"\t.long 0,0"
            ,"\t.long 0,0"
            ,"\t.long 0,0"
            ,"\t.long 0,0"
            ,"\t.long 0,0"
            ,"\t.long 0,0"
            ,"\t.long 0,0"
            ,"\t.long 0,0"
            ,"\t.long 0,0"
            ,"\t.long 0,0"
            ,"\t.long 0,0"
            ,"\t.long 0,0"
            ,""
            ,"\t.section .rodata"
            ,"\t.globl __wine_spec_file_name"
            ,"\t.hidden __wine_spec_file_name"
            ,"__wine_spec_file_name:"
            ,".L__wine_spec_file_name:"
            ,"\t.string \"portwine.exe\""
            ,""
            ,"\t.section \".init\",\"ax\""
            ,"\tcall __wine_spec_init_ctor"
            ,""
            ]
      ls2 = ["/* import table */"
            ,""
            ,"\t.data"
            ,"\t.align 4"
            ,".L__wine_spec_imports:"
            ,"\t.long .L__wine_spec_import_data_names+0-.L__wine_spec_rva_base"
            ,"\t.long 0"
            ,"\t.long 0"
            ]++
            mkImports 0 importedFunctions++
            ["\t.long 0"
            ,"\t.long 0"
            ]
      ls3 = [""
            ,"\t.align 8"
            ,".L__wine_spec_import_data_names:"
            ]++
            concatMap mkDataNames importedFunctions
      ls4 = mkTable importedFunctions
  ls5 <- fmap concat (mapM mkImportData importedFunctions)
  let ls6 = concatMap mkImportName importedFunctions
      (_,lls) = mapAccumL mkFunctions 0 importedFunctions
      ls7 = [""
            ,"/* immediate import thunks */"
            ,""
            ,"\t.text"
            ,"\t.align 8"
            ,"__wine_spec_import_thunks:"
            ,""]++
            concat lls
      ls8 = [""
            ,""
            ,"\t.size __wine_spec_import_thunks, .-__wine_spec_import_thunks"
            ,"\t.section .note.GNU-stack,\"\",@progbits"
            ]
  putStrLn (unlines (ls1++ls2++ls3++ls4++ls5++ls6++ls7++ls8))

ptr | sizeOf nullPtr == 8 = ".quad"
    | otherwise           = ".long"

mkImports offs []              = []    
mkImports offs ((dll,fs):dlls) =
  let offs' = offs+length fs+1
  in ["\t.long .L__wine_spec_import_name_"++dll++"_dll-.L__wine_spec_rva_base"
     ,"\t.long .L__wine_spec_import_data_ptrs+"++show (offs*(sizeOf nullPtr))++"-.L__wine_spec_rva_base"
     ,if null dlls
        then "\t.long 0"
        else "\t.long .L__wine_spec_import_data_names+"++show (offs'*(sizeOf nullPtr))++"-.L__wine_spec_rva_base"
     ,"\t.long 0"
     ,"\t.long 0"
     ]++
     mkImports offs' dlls

mkDataNames (dll,fs) = map mkDataName fs ++ ["\t"++ptr++" 0"]
  where
    mkDataName f =
      "\t"++ptr++" .L__wine_spec_import_data_"++dll++"_dll_"++f++"-.L__wine_spec_rva_base"

mkTable fns =
  let count = sum (map ((+1) . length . snd) fns)
  in [".L__wine_spec_import_data_ptrs:"]++
     replicate count ("\t"++ptr++" 0")++
     [".L__wine_spec_imports_end:"]

mkImportData (dll,fs) = do
  defs <- fmap (concatMap toEntry . lines) (readFile ("/usr/lib/x86_64-linux-gnu/wine/lib"++dll++".def"))
  return (concat [mkData f id | f <- fs, let Just id = lookup f defs])
  where
    toEntry l =
      case words l of
        [f,'@':s] -> [(f,read s :: Int)]
        _         -> []
    mkData f id =
      ["\t.align 2"
      ,".L__wine_spec_import_data_"++dll++"_dll_"++f++":"
      ,"\t.short "++show id
      ,"\t.string \""++f++"\""
      ]

mkImportName (dll,fs) =
  [".L__wine_spec_import_name_"++dll++"_dll:"
  ,"\t.string \""++dll++".dll\""
  ]
  
mkFunctions offs (dll,fs) =
  let (offs',lls) = mapAccumL mkFunction offs fs
  in (offs'+1,concat lls)
  where
    mkFunction offs f = 
      (offs+1,["\t.align 4"
              ,"\t.type "++f++",@function"
              ,"\t.globl "++f
              ,"\t.hidden "++f
              ,f++":"
              ,"\tjmpq *.L__wine_spec_import_data_ptrs+"++show (offs*sizeOf nullPtr)++"(%rip)"
              ,"\t.size "++f++", .-"++f
              ,""
              ])

importedFunctions =
  [("comctl32", ["ImageList_Add"
                ,"ImageList_Create"
                ,"ImageList_Draw"
                ,"ImageList_GetImageInfo"
                ,"ImageList_AddMasked"
                ,"ImageList_Destroy"
                ,"InitCommonControlsEx"
                ,"PropertySheetA"
                ])
  ,("shell32",  ["SHBrowseForFolderA"
                ,"SHGetPathFromIDListA"
                ])
  ,("comdlg32", ["ChooseColorA"
                ,"ChooseFontA"
                ,"GetOpenFileNameA"
                ,"GetSaveFileNameA"
                ])
  ,("gdi32",    ["Arc"
                ,"BitBlt"
                ,"CombineRgn"
                ,"CreateBitmap"
                ,"CreateBrushIndirect"
                ,"CreateCompatibleBitmap"
                ,"CreateCompatibleDC"
                ,"CreateDCA"
                ,"CreateDIBitmap"
                ,"CreateFontIndirectA"
                ,"CreatePatternBrush"
                ,"CreatePen"
                ,"CreateRectRgn"
                ,"CreateRectRgnIndirect"
                ,"CreateSolidBrush"
                ,"DeleteDC"
                ,"DeleteObject"
                ,"Ellipse"
                ,"EnumFontFamiliesA"
                ,"ExcludeClipRect"
                ,"ExtCreatePen"
                ,"ExtTextOutA"
                ,"ExtTextOutW"
                ,"FillRgn"
                ,"GetBkColor"
                ,"GetClipBox"
                ,"GetClipRgn"
                ,"GetCurrentObject"
                ,"GetDeviceCaps"
                ,"GetDIBits"
                ,"GetMapMode"
                ,"GetObjectA"
                ,"GetStockObject"
                ,"GetTextColor"
                ,"GetTextExtentPoint32A"
                ,"GetTextMetricsA"
                ,"IntersectClipRect"
                ,"LineTo"
                ,"ModifyWorldTransform"
                ,"MoveToEx"
                ,"PatBlt"
                ,"Pie"
                ,"Polygon"
                ,"Polyline"
                ,"Rectangle"
                ,"RestoreDC"
                ,"SaveDC"
                ,"SelectClipRgn"
                ,"SelectObject"
                ,"SetBkColor"
                ,"SetBkMode"
                ,"SetBrushOrgEx"
                ,"SetGraphicsMode"
                ,"SetMapMode"
                ,"SetPixelV"
                ,"SetPolyFillMode"
                ,"SetRectRgn"
                ,"SetROP2"
                ,"SetStretchBltMode"
                ,"SetTextAlign"
                ,"SetTextColor"
                ,"SetViewportExtEx"
                ,"SetViewportOrgEx"
                ,"SetWindowExtEx"
                ,"StretchBlt"
                ,"TextOutA"
                ,"GetRgnBox"
                ,"SetDIBits"
                ])
  ,("advapi32", ["RegCloseKey"
                ,"RegCreateKeyExW"
                ,"RegOpenKeyExW"
                ,"RegQueryValueExW"
                ,"RegSetValueExW"
                ])
  ,("user32",   ["AdjustWindowRect"
                ,"AdjustWindowRectEx"
                ,"AppendMenuA"
                ,"BeginPaint"
                ,"CallWindowProcA"
                ,"ClientToScreen"
                ,"CreateAcceleratorTableA"
                ,"CreateDialogIndirectParamA"
                ,"CreateMenu"
                ,"CreatePopupMenu"
                ,"CreateWindowExA"
                ,"CreateWindowExW"
                ,"DefDlgProcA"
                ,"DefFrameProcW"
                ,"DefMDIChildProcA"
                ,"DefWindowProcA"
                ,"DefWindowProcW"
                ,"DeleteMenu"
                ,"DestroyAcceleratorTable"
                ,"DestroyWindow"
                ,"DialogBoxIndirectParamA"
                ,"DispatchMessageA"
                ,"DrawEdge"
                ,"DrawFocusRect"
                ,"DrawFrameControl"
                ,"DrawMenuBar"
                ,"DrawTextA"
                ,"DrawTextW"
                ,"EnableMenuItem"
                ,"EnableWindow"
                ,"EndDialog"
                ,"EndPaint"
                ,"FillRect"
                ,"GetAsyncKeyState"
                ,"GetCapture"
                ,"GetClassInfoW"
                ,"GetClassLongPtrA"
                ,"GetClassNameA"
                ,"GetClassNameW"
                ,"GetClientRect"
                ,"GetCursorPos"
                ,"GetDC"
                ,"GetDCEx"
                ,"GetDesktopWindow"
                ,"GetDialogBaseUnits"
                ,"GetKeyState"
                ,"GetMenu"
                ,"GetMenuCheckMarkDimensions"
                ,"GetMenuItemInfoA"
                ,"GetMessageA"
                ,"GetParent"
                ,"GetSysColor"
                ,"GetSysColorBrush"
                ,"GetSystemMenu"
                ,"GetSystemMetrics"
                ,"GetTopWindow"
                ,"GetWindow"
                ,"GetWindowDC"
                ,"GetWindowLongPtrW"
                ,"GetWindowPlacement"
                ,"GetWindowRect"
                ,"GetWindowTextA"
                ,"GetWindowTextLengthA"
                ,"GetWindowTextLengthW"
                ,"GetWindowTextW"
                ,"InflateRect"
                ,"InsertMenuA"
                ,"InsertMenuItemA"
                ,"IntersectRect"
                ,"InvalidateRect"
                ,"IsDialogMessageA"
                ,"IsIconic"
                ,"IsWindow"
                ,"IsWindowEnabled"
                ,"IsWindowVisible"
                ,"KillTimer"
                ,"LoadCursorA"
                ,"LoadIconA"
                ,"LoadImageA"
                ,"LockWindowUpdate"
                ,"MapWindowPoints"
                ,"MessageBoxW"
                ,"MoveWindow"
                ,"OffsetRect"
                ,"PeekMessageA"
                ,"PostQuitMessage"
                ,"PtInRect"
                ,"RegisterClassA"
                ,"RegisterClassW"
                ,"ReleaseCapture"
                ,"ReleaseDC"
                ,"ScreenToClient"
                ,"SendDlgItemMessageA"
                ,"SendMessageA"
                ,"SendMessageW"
                ,"PostMessageW"
                ,"SetActiveWindow"
                ,"SetCapture"
                ,"SetCursor"
                ,"SetFocus"
                ,"SetMenu"
                ,"SetMenuItemInfoA"
                ,"SetParent"
                ,"SetRectEmpty"
                ,"SetScrollInfo"
                ,"SetScrollPos"
                ,"SetTimer"
                ,"SetWindowLongPtrW"
                ,"SetWindowPos"
                ,"SetWindowTextA"
                ,"SetWindowTextW"
                ,"ShowWindow"
                ,"SystemParametersInfoA"
                ,"TrackPopupMenu"
                ,"TranslateAcceleratorA"
                ,"TranslateMDISysAccel"
                ,"TranslateMessage"
                ,"UpdateWindow"
                ,"RedrawWindow"
                ,"IsWindowUnicode"
                ])
  ,("kernel32", ["CloseHandle"
                ,"CreateFileA"
                ,"ExitProcess"
                ,"FreeLibrary"
                ,"GetCommandLineA"
                ,"GetLocaleInfoA"
                ,"GetModuleHandleA"
                ,"GetProcAddress"
                ,"GetStartupInfoA"
                ,"LoadLibraryA"
                ,"MulDiv"
                ,"MultiByteToWideChar"
                ,"WideCharToMultiByte"
                ,"WriteFile"
                ,"CreateSemaphoreA"
                ,"ReleaseSemaphore"
                ,"WaitForSingleObject"
                ,"GlobalAlloc"
                ,"GlobalFree"
                ,"GetTickCount"
                ,"Sleep"
                ,"DisableThreadLibraryCalls"
                ,"lstrcmpW"
                ])
  ,("ole32",    ["OleInitialize"
                ,"OleUninitialize"
                ,"CoTaskMemAlloc"
                ,"CoTaskMemFree"
                ,"CoCreateInstance"
                ])
  ,("oleaut32", ["VariantInit"
                ,"VariantClear"
                ,"SysAllocString"
                ,"SysFreeString"
                ,"SafeArrayCreate"
                ,"SafeArrayDestroy"
                ,"SafeArrayAccessData"
                ])
  ]

