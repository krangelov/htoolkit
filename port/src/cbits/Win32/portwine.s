/* File generated automatically; do not edit! */
/* This file can be copied, modified and distributed without restriction. */


	.section ".init","ax"
	jmp 1f
__wine_spec_pe_header:
	.skip 69632
1:

	.data
	.align 8
	.globl __wine_spec_nt_header
	.hidden __wine_spec_nt_header
__wine_spec_nt_header:
.L__wine_spec_rva_base:
	.long 0x4550
	.short 0x8664
	.short 0
	.long 0
	.long 0
	.long 0
	.short 240
	.short 0x0022
	.short 0x020b
	.byte 0
	.byte 0
	.long 0
	.long 0
	.long 0
	.quad __wine_spec_exe_entry
	.quad __wine_spec_pe_header
	.long 4096
	.long 4096
	.short 1,0
	.short 0,0
	.short 4,0
	.long 0
	.long _end-.L__wine_spec_rva_base
	.long 4096
	.long 0
	.short 0x0002
	.short 0x0100
	.quad 1048576,4096
	.quad 1048576,4096
	.long 0
	.long 16
	.long 0,0
	.long .L__wine_spec_imports-.L__wine_spec_rva_base,.L__wine_spec_imports_end-.L__wine_spec_imports
	.long 0,0
	.long 0,0
	.long 0,0
	.long 0,0
	.long 0,0
	.long 0,0
	.long 0,0
	.long 0,0
	.long 0,0
	.long 0,0
	.long 0,0
	.long 0,0
	.long 0,0
	.long 0,0

	.section .rodata
	.globl __wine_spec_file_name
	.hidden __wine_spec_file_name
__wine_spec_file_name:
.L__wine_spec_file_name:
	.string "portwine.exe"

	.section ".init","ax"
	call __wine_spec_init_ctor

/* import table */

	.data
	.align 4
.L__wine_spec_imports:
	.long .L__wine_spec_import_data_names+0-.L__wine_spec_rva_base
	.long 0
	.long 0
	.long .L__wine_spec_import_name_comctl32_dll-.L__wine_spec_rva_base
	.long .L__wine_spec_import_data_ptrs+0-.L__wine_spec_rva_base
	.long .L__wine_spec_import_data_names+72-.L__wine_spec_rva_base
	.long 0
	.long 0
	.long .L__wine_spec_import_name_shell32_dll-.L__wine_spec_rva_base
	.long .L__wine_spec_import_data_ptrs+72-.L__wine_spec_rva_base
	.long .L__wine_spec_import_data_names+96-.L__wine_spec_rva_base
	.long 0
	.long 0
	.long .L__wine_spec_import_name_comdlg32_dll-.L__wine_spec_rva_base
	.long .L__wine_spec_import_data_ptrs+96-.L__wine_spec_rva_base
	.long .L__wine_spec_import_data_names+136-.L__wine_spec_rva_base
	.long 0
	.long 0
	.long .L__wine_spec_import_name_gdi32_dll-.L__wine_spec_rva_base
	.long .L__wine_spec_import_data_ptrs+136-.L__wine_spec_rva_base
	.long .L__wine_spec_import_data_names+688-.L__wine_spec_rva_base
	.long 0
	.long 0
	.long .L__wine_spec_import_name_advapi32_dll-.L__wine_spec_rva_base
	.long .L__wine_spec_import_data_ptrs+688-.L__wine_spec_rva_base
	.long .L__wine_spec_import_data_names+736-.L__wine_spec_rva_base
	.long 0
	.long 0
	.long .L__wine_spec_import_name_user32_dll-.L__wine_spec_rva_base
	.long .L__wine_spec_import_data_ptrs+736-.L__wine_spec_rva_base
	.long .L__wine_spec_import_data_names+1720-.L__wine_spec_rva_base
	.long 0
	.long 0
	.long .L__wine_spec_import_name_kernel32_dll-.L__wine_spec_rva_base
	.long .L__wine_spec_import_data_ptrs+1720-.L__wine_spec_rva_base
	.long .L__wine_spec_import_data_names+1912-.L__wine_spec_rva_base
	.long 0
	.long 0
	.long .L__wine_spec_import_name_ole32_dll-.L__wine_spec_rva_base
	.long .L__wine_spec_import_data_ptrs+1912-.L__wine_spec_rva_base
	.long 0
	.long 0
	.long 0
	.long 0
	.long 0

	.align 8
.L__wine_spec_import_data_names:
	.quad .L__wine_spec_import_data_comctl32_dll_ImageList_Add-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_comctl32_dll_ImageList_Create-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_comctl32_dll_ImageList_Draw-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_comctl32_dll_ImageList_GetImageInfo-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_comctl32_dll_ImageList_AddMasked-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_comctl32_dll_ImageList_Destroy-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_comctl32_dll_InitCommonControlsEx-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_comctl32_dll_PropertySheetA-.L__wine_spec_rva_base
	.quad 0
	.quad .L__wine_spec_import_data_shell32_dll_SHBrowseForFolderA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_shell32_dll_SHGetPathFromIDListA-.L__wine_spec_rva_base
	.quad 0
	.quad .L__wine_spec_import_data_comdlg32_dll_ChooseColorA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_comdlg32_dll_ChooseFontA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_comdlg32_dll_GetOpenFileNameA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_comdlg32_dll_GetSaveFileNameA-.L__wine_spec_rva_base
	.quad 0
	.quad .L__wine_spec_import_data_gdi32_dll_Arc-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_BitBlt-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_CombineRgn-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_CreateBitmap-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_CreateBrushIndirect-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_CreateCompatibleBitmap-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_CreateCompatibleDC-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_CreateDCA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_CreateDIBitmap-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_CreateFontIndirectA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_CreatePatternBrush-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_CreatePen-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_CreateRectRgn-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_CreateRectRgnIndirect-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_CreateSolidBrush-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_DeleteDC-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_DeleteObject-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_Ellipse-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_EnumFontFamiliesA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_ExcludeClipRect-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_ExtCreatePen-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_ExtTextOutA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_ExtTextOutW-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_FillRgn-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_GetBkColor-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_GetClipBox-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_GetClipRgn-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_GetCurrentObject-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_GetDeviceCaps-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_GetDIBits-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_GetMapMode-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_GetObjectA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_GetStockObject-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_GetTextColor-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_GetTextExtentPoint32A-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_GetTextMetricsA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_IntersectClipRect-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_LineTo-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_ModifyWorldTransform-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_MoveToEx-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_PatBlt-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_Pie-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_Polygon-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_Polyline-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_Rectangle-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_RestoreDC-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_SaveDC-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_SelectClipRgn-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_SelectObject-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_SetBkColor-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_SetBkMode-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_SetBrushOrgEx-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_SetGraphicsMode-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_SetMapMode-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_SetPixelV-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_SetPolyFillMode-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_SetRectRgn-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_SetROP2-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_SetStretchBltMode-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_SetTextAlign-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_SetTextColor-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_SetViewportExtEx-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_SetViewportOrgEx-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_SetWindowExtEx-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_StretchBlt-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_TextOutA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_GetRgnBox-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_gdi32_dll_SetDIBits-.L__wine_spec_rva_base
	.quad 0
	.quad .L__wine_spec_import_data_advapi32_dll_RegCloseKey-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_advapi32_dll_RegCreateKeyExW-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_advapi32_dll_RegOpenKeyExW-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_advapi32_dll_RegQueryValueExW-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_advapi32_dll_RegSetValueExW-.L__wine_spec_rva_base
	.quad 0
	.quad .L__wine_spec_import_data_user32_dll_AdjustWindowRect-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_AdjustWindowRectEx-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_AppendMenuA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_BeginPaint-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_CallWindowProcA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_ClientToScreen-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_CreateAcceleratorTableA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_CreateDialogIndirectParamA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_CreateMenu-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_CreatePopupMenu-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_CreateWindowExA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_CreateWindowExW-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_DefDlgProcA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_DefFrameProcW-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_DefMDIChildProcA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_DefWindowProcA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_DefWindowProcW-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_DeleteMenu-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_DestroyAcceleratorTable-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_DestroyWindow-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_DialogBoxIndirectParamA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_DispatchMessageA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_DrawEdge-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_DrawFocusRect-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_DrawFrameControl-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_DrawMenuBar-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_DrawTextA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_DrawTextW-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_EnableMenuItem-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_EnableWindow-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_EndDialog-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_EndPaint-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_FillRect-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetAsyncKeyState-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetCapture-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetClassInfoW-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetClassLongPtrA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetClassNameA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetClassNameW-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetClientRect-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetCursorPos-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetDC-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetDCEx-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetDesktopWindow-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetDialogBaseUnits-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetKeyState-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetMenu-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetMenuCheckMarkDimensions-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetMenuItemInfoA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetMessageA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetParent-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetSysColor-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetSysColorBrush-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetSystemMenu-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetSystemMetrics-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetTopWindow-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetWindow-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetWindowDC-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetWindowLongA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetWindowLongPtrA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetWindowPlacement-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetWindowRect-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetWindowTextA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetWindowTextLengthA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetWindowTextLengthW-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_GetWindowTextW-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_InflateRect-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_InsertMenuA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_InsertMenuItemA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_IntersectRect-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_InvalidateRect-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_IsDialogMessageA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_IsIconic-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_IsWindow-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_IsWindowEnabled-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_IsWindowVisible-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_KillTimer-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_LoadCursorA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_LoadIconA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_LoadImageA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_LockWindowUpdate-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_MapWindowPoints-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_MessageBoxW-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_MoveWindow-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_OffsetRect-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_PeekMessageA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_PostQuitMessage-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_PtInRect-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_RegisterClassA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_RegisterClassW-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_ReleaseCapture-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_ReleaseDC-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_ScreenToClient-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SendDlgItemMessageA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SendMessageA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SendMessageW-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_PostMessageW-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SetActiveWindow-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SetCapture-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SetCursor-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SetFocus-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SetMenu-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SetMenuItemInfoA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SetParent-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SetRectEmpty-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SetScrollInfo-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SetScrollPos-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SetTimer-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SetWindowLongA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SetWindowLongPtrA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SetWindowPos-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SetWindowTextA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SetWindowTextW-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_ShowWindow-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_SystemParametersInfoA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_TrackPopupMenu-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_TranslateAcceleratorA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_TranslateMDISysAccel-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_TranslateMessage-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_UpdateWindow-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_RedrawWindow-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_user32_dll_IsWindowUnicode-.L__wine_spec_rva_base
	.quad 0
	.quad .L__wine_spec_import_data_kernel32_dll_CloseHandle-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_CreateFileA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_ExitProcess-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_FreeLibrary-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_GetCommandLineA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_GetLocaleInfoA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_GetModuleHandleA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_GetProcAddress-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_GetStartupInfoA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_LoadLibraryA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_MulDiv-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_MultiByteToWideChar-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_WideCharToMultiByte-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_WriteFile-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_CreateSemaphoreA-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_ReleaseSemaphore-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_WaitForSingleObject-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_GlobalAlloc-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_GlobalFree-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_GetTickCount-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_Sleep-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_DisableThreadLibraryCalls-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_kernel32_dll_lstrcmpW-.L__wine_spec_rva_base
	.quad 0
	.quad .L__wine_spec_import_data_ole32_dll_CoTaskMemFree-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_ole32_dll_CoInitialize-.L__wine_spec_rva_base
	.quad .L__wine_spec_import_data_ole32_dll_CoUninitialize-.L__wine_spec_rva_base
	.quad 0
.L__wine_spec_import_data_ptrs:
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
	.quad 0
.L__wine_spec_imports_end:
	.align 2
.L__wine_spec_import_data_comctl32_dll_ImageList_Add:
	.short 42
	.string "ImageList_Add"
	.align 2
.L__wine_spec_import_data_comctl32_dll_ImageList_Create:
	.short 48
	.string "ImageList_Create"
	.align 2
.L__wine_spec_import_data_comctl32_dll_ImageList_Draw:
	.short 54
	.string "ImageList_Draw"
	.align 2
.L__wine_spec_import_data_comctl32_dll_ImageList_GetImageInfo:
	.short 65
	.string "ImageList_GetImageInfo"
	.align 2
.L__wine_spec_import_data_comctl32_dll_ImageList_AddMasked:
	.short 44
	.string "ImageList_AddMasked"
	.align 2
.L__wine_spec_import_data_comctl32_dll_ImageList_Destroy:
	.short 49
	.string "ImageList_Destroy"
	.align 2
.L__wine_spec_import_data_comctl32_dll_InitCommonControlsEx:
	.short 87
	.string "InitCommonControlsEx"
	.align 2
.L__wine_spec_import_data_comctl32_dll_PropertySheetA:
	.short 91
	.string "PropertySheetA"
	.align 2
.L__wine_spec_import_data_shell32_dll_SHBrowseForFolderA:
	.short 273
	.string "SHBrowseForFolderA"
	.align 2
.L__wine_spec_import_data_shell32_dll_SHGetPathFromIDListA:
	.short 324
	.string "SHGetPathFromIDListA"
	.align 2
.L__wine_spec_import_data_comdlg32_dll_ChooseColorA:
	.short 1
	.string "ChooseColorA"
	.align 2
.L__wine_spec_import_data_comdlg32_dll_ChooseFontA:
	.short 3
	.string "ChooseFontA"
	.align 2
.L__wine_spec_import_data_comdlg32_dll_GetOpenFileNameA:
	.short 13
	.string "GetOpenFileNameA"
	.align 2
.L__wine_spec_import_data_comdlg32_dll_GetSaveFileNameA:
	.short 15
	.string "GetSaveFileNameA"
	.align 2
.L__wine_spec_import_data_gdi32_dll_Arc:
	.short 115
	.string "Arc"
	.align 2
.L__wine_spec_import_data_gdi32_dll_BitBlt:
	.short 118
	.string "BitBlt"
	.align 2
.L__wine_spec_import_data_gdi32_dll_CombineRgn:
	.short 129
	.string "CombineRgn"
	.align 2
.L__wine_spec_import_data_gdi32_dll_CreateBitmap:
	.short 135
	.string "CreateBitmap"
	.align 2
.L__wine_spec_import_data_gdi32_dll_CreateBrushIndirect:
	.short 137
	.string "CreateBrushIndirect"
	.align 2
.L__wine_spec_import_data_gdi32_dll_CreateCompatibleBitmap:
	.short 140
	.string "CreateCompatibleBitmap"
	.align 2
.L__wine_spec_import_data_gdi32_dll_CreateCompatibleDC:
	.short 141
	.string "CreateCompatibleDC"
	.align 2
.L__wine_spec_import_data_gdi32_dll_CreateDCA:
	.short 142
	.string "CreateDCA"
	.align 2
.L__wine_spec_import_data_gdi32_dll_CreateDIBitmap:
	.short 147
	.string "CreateDIBitmap"
	.align 2
.L__wine_spec_import_data_gdi32_dll_CreateFontIndirectA:
	.short 154
	.string "CreateFontIndirectA"
	.align 2
.L__wine_spec_import_data_gdi32_dll_CreatePatternBrush:
	.short 166
	.string "CreatePatternBrush"
	.align 2
.L__wine_spec_import_data_gdi32_dll_CreatePen:
	.short 167
	.string "CreatePen"
	.align 2
.L__wine_spec_import_data_gdi32_dll_CreateRectRgn:
	.short 171
	.string "CreateRectRgn"
	.align 2
.L__wine_spec_import_data_gdi32_dll_CreateRectRgnIndirect:
	.short 172
	.string "CreateRectRgnIndirect"
	.align 2
.L__wine_spec_import_data_gdi32_dll_CreateSolidBrush:
	.short 176
	.string "CreateSolidBrush"
	.align 2
.L__wine_spec_import_data_gdi32_dll_DeleteDC:
	.short 179
	.string "DeleteDC"
	.align 2
.L__wine_spec_import_data_gdi32_dll_DeleteObject:
	.short 182
	.string "DeleteObject"
	.align 2
.L__wine_spec_import_data_gdi32_dll_Ellipse:
	.short 188
	.string "Ellipse"
	.align 2
.L__wine_spec_import_data_gdi32_dll_EnumFontFamiliesA:
	.short 194
	.string "EnumFontFamiliesA"
	.align 2
.L__wine_spec_import_data_gdi32_dll_ExcludeClipRect:
	.short 206
	.string "ExcludeClipRect"
	.align 2
.L__wine_spec_import_data_gdi32_dll_ExtCreatePen:
	.short 207
	.string "ExtCreatePen"
	.align 2
.L__wine_spec_import_data_gdi32_dll_ExtTextOutA:
	.short 212
	.string "ExtTextOutA"
	.align 2
.L__wine_spec_import_data_gdi32_dll_ExtTextOutW:
	.short 213
	.string "ExtTextOutW"
	.align 2
.L__wine_spec_import_data_gdi32_dll_FillRgn:
	.short 215
	.string "FillRgn"
	.align 2
.L__wine_spec_import_data_gdi32_dll_GetBkColor:
	.short 294
	.string "GetBkColor"
	.align 2
.L__wine_spec_import_data_gdi32_dll_GetClipBox:
	.short 313
	.string "GetClipBox"
	.align 2
.L__wine_spec_import_data_gdi32_dll_GetClipRgn:
	.short 314
	.string "GetClipRgn"
	.align 2
.L__wine_spec_import_data_gdi32_dll_GetCurrentObject:
	.short 317
	.string "GetCurrentObject"
	.align 2
.L__wine_spec_import_data_gdi32_dll_GetDeviceCaps:
	.short 324
	.string "GetDeviceCaps"
	.align 2
.L__wine_spec_import_data_gdi32_dll_GetDIBits:
	.short 323
	.string "GetDIBits"
	.align 2
.L__wine_spec_import_data_gdi32_dll_GetMapMode:
	.short 354
	.string "GetMapMode"
	.align 2
.L__wine_spec_import_data_gdi32_dll_GetObjectA:
	.short 362
	.string "GetObjectA"
	.align 2
.L__wine_spec_import_data_gdi32_dll_GetStockObject:
	.short 378
	.string "GetStockObject"
	.align 2
.L__wine_spec_import_data_gdi32_dll_GetTextColor:
	.short 386
	.string "GetTextColor"
	.align 2
.L__wine_spec_import_data_gdi32_dll_GetTextExtentPoint32A:
	.short 390
	.string "GetTextExtentPoint32A"
	.align 2
.L__wine_spec_import_data_gdi32_dll_GetTextMetricsA:
	.short 397
	.string "GetTextMetricsA"
	.align 2
.L__wine_spec_import_data_gdi32_dll_IntersectClipRect:
	.short 406
	.string "IntersectClipRect"
	.align 2
.L__wine_spec_import_data_gdi32_dll_LineTo:
	.short 410
	.string "LineTo"
	.align 2
.L__wine_spec_import_data_gdi32_dll_ModifyWorldTransform:
	.short 415
	.string "ModifyWorldTransform"
	.align 2
.L__wine_spec_import_data_gdi32_dll_MoveToEx:
	.short 416
	.string "MoveToEx"
	.align 2
.L__wine_spec_import_data_gdi32_dll_PatBlt:
	.short 423
	.string "PatBlt"
	.align 2
.L__wine_spec_import_data_gdi32_dll_Pie:
	.short 425
	.string "Pie"
	.align 2
.L__wine_spec_import_data_gdi32_dll_Polygon:
	.short 438
	.string "Polygon"
	.align 2
.L__wine_spec_import_data_gdi32_dll_Polyline:
	.short 439
	.string "Polyline"
	.align 2
.L__wine_spec_import_data_gdi32_dll_Rectangle:
	.short 446
	.string "Rectangle"
	.align 2
.L__wine_spec_import_data_gdi32_dll_RestoreDC:
	.short 456
	.string "RestoreDC"
	.align 2
.L__wine_spec_import_data_gdi32_dll_SaveDC:
	.short 458
	.string "SaveDC"
	.align 2
.L__wine_spec_import_data_gdi32_dll_SelectClipRgn:
	.short 463
	.string "SelectClipRgn"
	.align 2
.L__wine_spec_import_data_gdi32_dll_SelectObject:
	.short 465
	.string "SelectObject"
	.align 2
.L__wine_spec_import_data_gdi32_dll_SetBkColor:
	.short 471
	.string "SetBkColor"
	.align 2
.L__wine_spec_import_data_gdi32_dll_SetBkMode:
	.short 472
	.string "SetBkMode"
	.align 2
.L__wine_spec_import_data_gdi32_dll_SetBrushOrgEx:
	.short 474
	.string "SetBrushOrgEx"
	.align 2
.L__wine_spec_import_data_gdi32_dll_SetGraphicsMode:
	.short 485
	.string "SetGraphicsMode"
	.align 2
.L__wine_spec_import_data_gdi32_dll_SetMapMode:
	.short 491
	.string "SetMapMode"
	.align 2
.L__wine_spec_import_data_gdi32_dll_SetPixelV:
	.short 500
	.string "SetPixelV"
	.align 2
.L__wine_spec_import_data_gdi32_dll_SetPolyFillMode:
	.short 501
	.string "SetPolyFillMode"
	.align 2
.L__wine_spec_import_data_gdi32_dll_SetRectRgn:
	.short 503
	.string "SetRectRgn"
	.align 2
.L__wine_spec_import_data_gdi32_dll_SetROP2:
	.short 502
	.string "SetROP2"
	.align 2
.L__wine_spec_import_data_gdi32_dll_SetStretchBltMode:
	.short 505
	.string "SetStretchBltMode"
	.align 2
.L__wine_spec_import_data_gdi32_dll_SetTextAlign:
	.short 507
	.string "SetTextAlign"
	.align 2
.L__wine_spec_import_data_gdi32_dll_SetTextColor:
	.short 509
	.string "SetTextColor"
	.align 2
.L__wine_spec_import_data_gdi32_dll_SetViewportExtEx:
	.short 511
	.string "SetViewportExtEx"
	.align 2
.L__wine_spec_import_data_gdi32_dll_SetViewportOrgEx:
	.short 512
	.string "SetViewportOrgEx"
	.align 2
.L__wine_spec_import_data_gdi32_dll_SetWindowExtEx:
	.short 515
	.string "SetWindowExtEx"
	.align 2
.L__wine_spec_import_data_gdi32_dll_StretchBlt:
	.short 521
	.string "StretchBlt"
	.align 2
.L__wine_spec_import_data_gdi32_dll_TextOutA:
	.short 526
	.string "TextOutA"
	.align 2
.L__wine_spec_import_data_gdi32_dll_GetRgnBox:
	.short 377
	.string "GetRgnBox"
	.align 2
.L__wine_spec_import_data_gdi32_dll_SetDIBits:
	.short 480
	.string "SetDIBits"
	.align 2
.L__wine_spec_import_data_advapi32_dll_RegCloseKey:
	.short 348
	.string "RegCloseKey"
	.align 2
.L__wine_spec_import_data_advapi32_dll_RegCreateKeyExW:
	.short 353
	.string "RegCreateKeyExW"
	.align 2
.L__wine_spec_import_data_advapi32_dll_RegOpenKeyExW:
	.short 383
	.string "RegOpenKeyExW"
	.align 2
.L__wine_spec_import_data_advapi32_dll_RegQueryValueExW:
	.short 393
	.string "RegQueryValueExW"
	.align 2
.L__wine_spec_import_data_advapi32_dll_RegSetValueExW:
	.short 405
	.string "RegSetValueExW"
	.align 2
.L__wine_spec_import_data_user32_dll_AdjustWindowRect:
	.short 2
	.string "AdjustWindowRect"
	.align 2
.L__wine_spec_import_data_user32_dll_AdjustWindowRectEx:
	.short 3
	.string "AdjustWindowRectEx"
	.align 2
.L__wine_spec_import_data_user32_dll_AppendMenuA:
	.short 8
	.string "AppendMenuA"
	.align 2
.L__wine_spec_import_data_user32_dll_BeginPaint:
	.short 13
	.string "BeginPaint"
	.align 2
.L__wine_spec_import_data_user32_dll_CallWindowProcA:
	.short 26
	.string "CallWindowProcA"
	.align 2
.L__wine_spec_import_data_user32_dll_ClientToScreen:
	.short 67
	.string "ClientToScreen"
	.align 2
.L__wine_spec_import_data_user32_dll_CreateAcceleratorTableA:
	.short 79
	.string "CreateAcceleratorTableA"
	.align 2
.L__wine_spec_import_data_user32_dll_CreateDialogIndirectParamA:
	.short 85
	.string "CreateDialogIndirectParamA"
	.align 2
.L__wine_spec_import_data_user32_dll_CreateMenu:
	.short 96
	.string "CreateMenu"
	.align 2
.L__wine_spec_import_data_user32_dll_CreatePopupMenu:
	.short 97
	.string "CreatePopupMenu"
	.align 2
.L__wine_spec_import_data_user32_dll_CreateWindowExA:
	.short 98
	.string "CreateWindowExA"
	.align 2
.L__wine_spec_import_data_user32_dll_CreateWindowExW:
	.short 99
	.string "CreateWindowExW"
	.align 2
.L__wine_spec_import_data_user32_dll_DefDlgProcA:
	.short 135
	.string "DefDlgProcA"
	.align 2
.L__wine_spec_import_data_user32_dll_DefFrameProcW:
	.short 138
	.string "DefFrameProcW"
	.align 2
.L__wine_spec_import_data_user32_dll_DefMDIChildProcA:
	.short 139
	.string "DefMDIChildProcA"
	.align 2
.L__wine_spec_import_data_user32_dll_DefWindowProcA:
	.short 142
	.string "DefWindowProcA"
	.align 2
.L__wine_spec_import_data_user32_dll_DefWindowProcW:
	.short 143
	.string "DefWindowProcW"
	.align 2
.L__wine_spec_import_data_user32_dll_DeleteMenu:
	.short 145
	.string "DeleteMenu"
	.align 2
.L__wine_spec_import_data_user32_dll_DestroyAcceleratorTable:
	.short 147
	.string "DestroyAcceleratorTable"
	.align 2
.L__wine_spec_import_data_user32_dll_DestroyWindow:
	.short 152
	.string "DestroyWindow"
	.align 2
.L__wine_spec_import_data_user32_dll_DialogBoxIndirectParamA:
	.short 153
	.string "DialogBoxIndirectParamA"
	.align 2
.L__wine_spec_import_data_user32_dll_DispatchMessageA:
	.short 159
	.string "DispatchMessageA"
	.align 2
.L__wine_spec_import_data_user32_dll_DrawEdge:
	.short 175
	.string "DrawEdge"
	.align 2
.L__wine_spec_import_data_user32_dll_DrawFocusRect:
	.short 176
	.string "DrawFocusRect"
	.align 2
.L__wine_spec_import_data_user32_dll_DrawFrameControl:
	.short 178
	.string "DrawFrameControl"
	.align 2
.L__wine_spec_import_data_user32_dll_DrawMenuBar:
	.short 181
	.string "DrawMenuBar"
	.align 2
.L__wine_spec_import_data_user32_dll_DrawTextA:
	.short 185
	.string "DrawTextA"
	.align 2
.L__wine_spec_import_data_user32_dll_DrawTextW:
	.short 188
	.string "DrawTextW"
	.align 2
.L__wine_spec_import_data_user32_dll_EnableMenuItem:
	.short 191
	.string "EnableMenuItem"
	.align 2
.L__wine_spec_import_data_user32_dll_EnableWindow:
	.short 193
	.string "EnableWindow"
	.align 2
.L__wine_spec_import_data_user32_dll_EndDialog:
	.short 195
	.string "EndDialog"
	.align 2
.L__wine_spec_import_data_user32_dll_EndPaint:
	.short 197
	.string "EndPaint"
	.align 2
.L__wine_spec_import_data_user32_dll_FillRect:
	.short 224
	.string "FillRect"
	.align 2
.L__wine_spec_import_data_user32_dll_GetAsyncKeyState:
	.short 240
	.string "GetAsyncKeyState"
	.align 2
.L__wine_spec_import_data_user32_dll_GetCapture:
	.short 241
	.string "GetCapture"
	.align 2
.L__wine_spec_import_data_user32_dll_GetClassInfoW:
	.short 247
	.string "GetClassInfoW"
	.align 2
.L__wine_spec_import_data_user32_dll_GetClassLongPtrA:
	.short 250
	.string "GetClassLongPtrA"
	.align 2
.L__wine_spec_import_data_user32_dll_GetClassNameA:
	.short 252
	.string "GetClassNameA"
	.align 2
.L__wine_spec_import_data_user32_dll_GetClassNameW:
	.short 253
	.string "GetClassNameW"
	.align 2
.L__wine_spec_import_data_user32_dll_GetClientRect:
	.short 255
	.string "GetClientRect"
	.align 2
.L__wine_spec_import_data_user32_dll_GetCursorPos:
	.short 267
	.string "GetCursorPos"
	.align 2
.L__wine_spec_import_data_user32_dll_GetDC:
	.short 268
	.string "GetDC"
	.align 2
.L__wine_spec_import_data_user32_dll_GetDCEx:
	.short 269
	.string "GetDCEx"
	.align 2
.L__wine_spec_import_data_user32_dll_GetDesktopWindow:
	.short 270
	.string "GetDesktopWindow"
	.align 2
.L__wine_spec_import_data_user32_dll_GetDialogBaseUnits:
	.short 271
	.string "GetDialogBaseUnits"
	.align 2
.L__wine_spec_import_data_user32_dll_GetKeyState:
	.short 291
	.string "GetKeyState"
	.align 2
.L__wine_spec_import_data_user32_dll_GetMenu:
	.short 302
	.string "GetMenu"
	.align 2
.L__wine_spec_import_data_user32_dll_GetMenuCheckMarkDimensions:
	.short 304
	.string "GetMenuCheckMarkDimensions"
	.align 2
.L__wine_spec_import_data_user32_dll_GetMenuItemInfoA:
	.short 311
	.string "GetMenuItemInfoA"
	.align 2
.L__wine_spec_import_data_user32_dll_GetMessageA:
	.short 317
	.string "GetMessageA"
	.align 2
.L__wine_spec_import_data_user32_dll_GetParent:
	.short 328
	.string "GetParent"
	.align 2
.L__wine_spec_import_data_user32_dll_GetSysColor:
	.short 348
	.string "GetSysColor"
	.align 2
.L__wine_spec_import_data_user32_dll_GetSysColorBrush:
	.short 349
	.string "GetSysColorBrush"
	.align 2
.L__wine_spec_import_data_user32_dll_GetSystemMenu:
	.short 350
	.string "GetSystemMenu"
	.align 2
.L__wine_spec_import_data_user32_dll_GetSystemMetrics:
	.short 351
	.string "GetSystemMetrics"
	.align 2
.L__wine_spec_import_data_user32_dll_GetTopWindow:
	.short 357
	.string "GetTopWindow"
	.align 2
.L__wine_spec_import_data_user32_dll_GetWindow:
	.short 363
	.string "GetWindow"
	.align 2
.L__wine_spec_import_data_user32_dll_GetWindowDC:
	.short 365
	.string "GetWindowDC"
	.align 2
.L__wine_spec_import_data_user32_dll_GetWindowLongA:
	.short 367
	.string "GetWindowLongA"
	.align 2
.L__wine_spec_import_data_user32_dll_GetWindowLongPtrA:
	.short 368
	.string "GetWindowLongPtrA"
	.align 2
.L__wine_spec_import_data_user32_dll_GetWindowPlacement:
	.short 374
	.string "GetWindowPlacement"
	.align 2
.L__wine_spec_import_data_user32_dll_GetWindowRect:
	.short 375
	.string "GetWindowRect"
	.align 2
.L__wine_spec_import_data_user32_dll_GetWindowTextA:
	.short 378
	.string "GetWindowTextA"
	.align 2
.L__wine_spec_import_data_user32_dll_GetWindowTextLengthA:
	.short 379
	.string "GetWindowTextLengthA"
	.align 2
.L__wine_spec_import_data_user32_dll_GetWindowTextLengthW:
	.short 380
	.string "GetWindowTextLengthW"
	.align 2
.L__wine_spec_import_data_user32_dll_GetWindowTextW:
	.short 381
	.string "GetWindowTextW"
	.align 2
.L__wine_spec_import_data_user32_dll_InflateRect:
	.short 391
	.string "InflateRect"
	.align 2
.L__wine_spec_import_data_user32_dll_InsertMenuA:
	.short 392
	.string "InsertMenuA"
	.align 2
.L__wine_spec_import_data_user32_dll_InsertMenuItemA:
	.short 393
	.string "InsertMenuItemA"
	.align 2
.L__wine_spec_import_data_user32_dll_IntersectRect:
	.short 397
	.string "IntersectRect"
	.align 2
.L__wine_spec_import_data_user32_dll_InvalidateRect:
	.short 398
	.string "InvalidateRect"
	.align 2
.L__wine_spec_import_data_user32_dll_IsDialogMessageA:
	.short 412
	.string "IsDialogMessageA"
	.align 2
.L__wine_spec_import_data_user32_dll_IsIconic:
	.short 417
	.string "IsIconic"
	.align 2
.L__wine_spec_import_data_user32_dll_IsWindow:
	.short 421
	.string "IsWindow"
	.align 2
.L__wine_spec_import_data_user32_dll_IsWindowEnabled:
	.short 422
	.string "IsWindowEnabled"
	.align 2
.L__wine_spec_import_data_user32_dll_IsWindowVisible:
	.short 424
	.string "IsWindowVisible"
	.align 2
.L__wine_spec_import_data_user32_dll_KillTimer:
	.short 427
	.string "KillTimer"
	.align 2
.L__wine_spec_import_data_user32_dll_LoadCursorA:
	.short 432
	.string "LoadCursorA"
	.align 2
.L__wine_spec_import_data_user32_dll_LoadIconA:
	.short 436
	.string "LoadIconA"
	.align 2
.L__wine_spec_import_data_user32_dll_LoadImageA:
	.short 438
	.string "LoadImageA"
	.align 2
.L__wine_spec_import_data_user32_dll_LockWindowUpdate:
	.short 452
	.string "LockWindowUpdate"
	.align 2
.L__wine_spec_import_data_user32_dll_MapWindowPoints:
	.short 462
	.string "MapWindowPoints"
	.align 2
.L__wine_spec_import_data_user32_dll_MessageBoxW:
	.short 474
	.string "MessageBoxW"
	.align 2
.L__wine_spec_import_data_user32_dll_MoveWindow:
	.short 480
	.string "MoveWindow"
	.align 2
.L__wine_spec_import_data_user32_dll_OffsetRect:
	.short 489
	.string "OffsetRect"
	.align 2
.L__wine_spec_import_data_user32_dll_PeekMessageA:
	.short 499
	.string "PeekMessageA"
	.align 2
.L__wine_spec_import_data_user32_dll_PostQuitMessage:
	.short 504
	.string "PostQuitMessage"
	.align 2
.L__wine_spec_import_data_user32_dll_PtInRect:
	.short 512
	.string "PtInRect"
	.align 2
.L__wine_spec_import_data_user32_dll_RegisterClassA:
	.short 519
	.string "RegisterClassA"
	.align 2
.L__wine_spec_import_data_user32_dll_RegisterClassW:
	.short 522
	.string "RegisterClassW"
	.align 2
.L__wine_spec_import_data_user32_dll_ReleaseCapture:
	.short 537
	.string "ReleaseCapture"
	.align 2
.L__wine_spec_import_data_user32_dll_ReleaseDC:
	.short 538
	.string "ReleaseDC"
	.align 2
.L__wine_spec_import_data_user32_dll_ScreenToClient:
	.short 545
	.string "ScreenToClient"
	.align 2
.L__wine_spec_import_data_user32_dll_SendDlgItemMessageA:
	.short 550
	.string "SendDlgItemMessageA"
	.align 2
.L__wine_spec_import_data_user32_dll_SendMessageA:
	.short 555
	.string "SendMessageA"
	.align 2
.L__wine_spec_import_data_user32_dll_SendMessageW:
	.short 560
	.string "SendMessageW"
	.align 2
.L__wine_spec_import_data_user32_dll_PostMessageW:
	.short 503
	.string "PostMessageW"
	.align 2
.L__wine_spec_import_data_user32_dll_SetActiveWindow:
	.short 564
	.string "SetActiveWindow"
	.align 2
.L__wine_spec_import_data_user32_dll_SetCapture:
	.short 565
	.string "SetCapture"
	.align 2
.L__wine_spec_import_data_user32_dll_SetCursor:
	.short 575
	.string "SetCursor"
	.align 2
.L__wine_spec_import_data_user32_dll_SetFocus:
	.short 584
	.string "SetFocus"
	.align 2
.L__wine_spec_import_data_user32_dll_SetMenu:
	.short 591
	.string "SetMenu"
	.align 2
.L__wine_spec_import_data_user32_dll_SetMenuItemInfoA:
	.short 596
	.string "SetMenuItemInfoA"
	.align 2
.L__wine_spec_import_data_user32_dll_SetParent:
	.short 600
	.string "SetParent"
	.align 2
.L__wine_spec_import_data_user32_dll_SetRectEmpty:
	.short 608
	.string "SetRectEmpty"
	.align 2
.L__wine_spec_import_data_user32_dll_SetScrollInfo:
	.short 609
	.string "SetScrollInfo"
	.align 2
.L__wine_spec_import_data_user32_dll_SetScrollPos:
	.short 610
	.string "SetScrollPos"
	.align 2
.L__wine_spec_import_data_user32_dll_SetTimer:
	.short 621
	.string "SetTimer"
	.align 2
.L__wine_spec_import_data_user32_dll_SetWindowLongA:
	.short 628
	.string "SetWindowLongA"
	.align 2
.L__wine_spec_import_data_user32_dll_SetWindowLongPtrA:
	.short 629
	.string "SetWindowLongPtrA"
	.align 2
.L__wine_spec_import_data_user32_dll_SetWindowPos:
	.short 633
	.string "SetWindowPos"
	.align 2
.L__wine_spec_import_data_user32_dll_SetWindowTextA:
	.short 636
	.string "SetWindowTextA"
	.align 2
.L__wine_spec_import_data_user32_dll_SetWindowTextW:
	.short 637
	.string "SetWindowTextW"
	.align 2
.L__wine_spec_import_data_user32_dll_ShowWindow:
	.short 648
	.string "ShowWindow"
	.align 2
.L__wine_spec_import_data_user32_dll_SystemParametersInfoA:
	.short 654
	.string "SystemParametersInfoA"
	.align 2
.L__wine_spec_import_data_user32_dll_TrackPopupMenu:
	.short 665
	.string "TrackPopupMenu"
	.align 2
.L__wine_spec_import_data_user32_dll_TranslateAcceleratorA:
	.short 668
	.string "TranslateAcceleratorA"
	.align 2
.L__wine_spec_import_data_user32_dll_TranslateMDISysAccel:
	.short 670
	.string "TranslateMDISysAccel"
	.align 2
.L__wine_spec_import_data_user32_dll_TranslateMessage:
	.short 671
	.string "TranslateMessage"
	.align 2
.L__wine_spec_import_data_user32_dll_UpdateWindow:
	.short 686
	.string "UpdateWindow"
	.align 2
.L__wine_spec_import_data_user32_dll_RedrawWindow:
	.short 518
	.string "RedrawWindow"
	.align 2
.L__wine_spec_import_data_user32_dll_IsWindowUnicode:
	.short 423
	.string "IsWindowUnicode"
	.align 2
.L__wine_spec_import_data_kernel32_dll_CloseHandle:
	.short 50
	.string "CloseHandle"
	.align 2
.L__wine_spec_import_data_kernel32_dll_CreateFileA:
	.short 88
	.string "CreateFileA"
	.align 2
.L__wine_spec_import_data_kernel32_dll_ExitProcess:
	.short 188
	.string "ExitProcess"
	.align 2
.L__wine_spec_import_data_kernel32_dll_FreeLibrary:
	.short 251
	.string "FreeLibrary"
	.align 2
.L__wine_spec_import_data_kernel32_dll_GetCommandLineA:
	.short 274
	.string "GetCommandLineA"
	.align 2
.L__wine_spec_import_data_kernel32_dll_GetLocaleInfoA:
	.short 375
	.string "GetLocaleInfoA"
	.align 2
.L__wine_spec_import_data_kernel32_dll_GetModuleHandleA:
	.short 388
	.string "GetModuleHandleA"
	.align 2
.L__wine_spec_import_data_kernel32_dll_GetProcAddress:
	.short 419
	.string "GetProcAddress"
	.align 2
.L__wine_spec_import_data_kernel32_dll_GetStartupInfoA:
	.short 444
	.string "GetStartupInfoA"
	.align 2
.L__wine_spec_import_data_kernel32_dll_LoadLibraryA:
	.short 634
	.string "LoadLibraryA"
	.align 2
.L__wine_spec_import_data_kernel32_dll_MulDiv:
	.short 668
	.string "MulDiv"
	.align 2
.L__wine_spec_import_data_kernel32_dll_MultiByteToWideChar:
	.short 669
	.string "MultiByteToWideChar"
	.align 2
.L__wine_spec_import_data_kernel32_dll_WideCharToMultiByte:
	.short 1007
	.string "WideCharToMultiByte"
	.align 2
.L__wine_spec_import_data_kernel32_dll_WriteFile:
	.short 1023
	.string "WriteFile"
	.align 2
.L__wine_spec_import_data_kernel32_dll_CreateSemaphoreA:
	.short 111
	.string "CreateSemaphoreA"
	.align 2
.L__wine_spec_import_data_kernel32_dll_ReleaseSemaphore:
	.short 793
	.string "ReleaseSemaphore"
	.align 2
.L__wine_spec_import_data_kernel32_dll_WaitForSingleObject:
	.short 999
	.string "WaitForSingleObject"
	.align 2
.L__wine_spec_import_data_kernel32_dll_GlobalAlloc:
	.short 514
	.string "GlobalAlloc"
	.align 2
.L__wine_spec_import_data_kernel32_dll_GlobalFree:
	.short 521
	.string "GlobalFree"
	.align 2
.L__wine_spec_import_data_kernel32_dll_GetTickCount:
	.short 486
	.string "GetTickCount"
	.align 2
.L__wine_spec_import_data_kernel32_dll_Sleep:
	.short 939
	.string "Sleep"
	.align 2
.L__wine_spec_import_data_kernel32_dll_DisableThreadLibraryCalls:
	.short 147
	.string "DisableThreadLibraryCalls"
	.align 2
.L__wine_spec_import_data_kernel32_dll_lstrcmpW:
	.short 1058
	.string "lstrcmpW"
	.align 2
.L__wine_spec_import_data_ole32_dll_CoTaskMemFree:
	.short 80
	.string "CoTaskMemFree"
	.align 2
.L__wine_spec_import_data_ole32_dll_CoInitialize:
	.short 45
	.string "CoInitialize"
	.align 2
.L__wine_spec_import_data_ole32_dll_CoUninitialize:
	.short 83
	.string "CoUninitialize"
.L__wine_spec_import_name_comctl32_dll:
	.string "comctl32.dll"
.L__wine_spec_import_name_shell32_dll:
	.string "shell32.dll"
.L__wine_spec_import_name_comdlg32_dll:
	.string "comdlg32.dll"
.L__wine_spec_import_name_gdi32_dll:
	.string "gdi32.dll"
.L__wine_spec_import_name_advapi32_dll:
	.string "advapi32.dll"
.L__wine_spec_import_name_user32_dll:
	.string "user32.dll"
.L__wine_spec_import_name_kernel32_dll:
	.string "kernel32.dll"
.L__wine_spec_import_name_ole32_dll:
	.string "ole32.dll"

/* immediate import thunks */

	.text
	.align 8
__wine_spec_import_thunks:

	.align 4
	.type ImageList_Add,@function
	.globl ImageList_Add
	.hidden ImageList_Add
ImageList_Add:
	jmpq *.L__wine_spec_import_data_ptrs+0(%rip)
	.size ImageList_Add, .-ImageList_Add

	.align 4
	.type ImageList_Create,@function
	.globl ImageList_Create
	.hidden ImageList_Create
ImageList_Create:
	jmpq *.L__wine_spec_import_data_ptrs+8(%rip)
	.size ImageList_Create, .-ImageList_Create

	.align 4
	.type ImageList_Draw,@function
	.globl ImageList_Draw
	.hidden ImageList_Draw
ImageList_Draw:
	jmpq *.L__wine_spec_import_data_ptrs+16(%rip)
	.size ImageList_Draw, .-ImageList_Draw

	.align 4
	.type ImageList_GetImageInfo,@function
	.globl ImageList_GetImageInfo
	.hidden ImageList_GetImageInfo
ImageList_GetImageInfo:
	jmpq *.L__wine_spec_import_data_ptrs+24(%rip)
	.size ImageList_GetImageInfo, .-ImageList_GetImageInfo

	.align 4
	.type ImageList_AddMasked,@function
	.globl ImageList_AddMasked
	.hidden ImageList_AddMasked
ImageList_AddMasked:
	jmpq *.L__wine_spec_import_data_ptrs+32(%rip)
	.size ImageList_AddMasked, .-ImageList_AddMasked

	.align 4
	.type ImageList_Destroy,@function
	.globl ImageList_Destroy
	.hidden ImageList_Destroy
ImageList_Destroy:
	jmpq *.L__wine_spec_import_data_ptrs+40(%rip)
	.size ImageList_Destroy, .-ImageList_Destroy

	.align 4
	.type InitCommonControlsEx,@function
	.globl InitCommonControlsEx
	.hidden InitCommonControlsEx
InitCommonControlsEx:
	jmpq *.L__wine_spec_import_data_ptrs+48(%rip)
	.size InitCommonControlsEx, .-InitCommonControlsEx

	.align 4
	.type PropertySheetA,@function
	.globl PropertySheetA
	.hidden PropertySheetA
PropertySheetA:
	jmpq *.L__wine_spec_import_data_ptrs+56(%rip)
	.size PropertySheetA, .-PropertySheetA

	.align 4
	.type SHBrowseForFolderA,@function
	.globl SHBrowseForFolderA
	.hidden SHBrowseForFolderA
SHBrowseForFolderA:
	jmpq *.L__wine_spec_import_data_ptrs+72(%rip)
	.size SHBrowseForFolderA, .-SHBrowseForFolderA

	.align 4
	.type SHGetPathFromIDListA,@function
	.globl SHGetPathFromIDListA
	.hidden SHGetPathFromIDListA
SHGetPathFromIDListA:
	jmpq *.L__wine_spec_import_data_ptrs+80(%rip)
	.size SHGetPathFromIDListA, .-SHGetPathFromIDListA

	.align 4
	.type ChooseColorA,@function
	.globl ChooseColorA
	.hidden ChooseColorA
ChooseColorA:
	jmpq *.L__wine_spec_import_data_ptrs+96(%rip)
	.size ChooseColorA, .-ChooseColorA

	.align 4
	.type ChooseFontA,@function
	.globl ChooseFontA
	.hidden ChooseFontA
ChooseFontA:
	jmpq *.L__wine_spec_import_data_ptrs+104(%rip)
	.size ChooseFontA, .-ChooseFontA

	.align 4
	.type GetOpenFileNameA,@function
	.globl GetOpenFileNameA
	.hidden GetOpenFileNameA
GetOpenFileNameA:
	jmpq *.L__wine_spec_import_data_ptrs+112(%rip)
	.size GetOpenFileNameA, .-GetOpenFileNameA

	.align 4
	.type GetSaveFileNameA,@function
	.globl GetSaveFileNameA
	.hidden GetSaveFileNameA
GetSaveFileNameA:
	jmpq *.L__wine_spec_import_data_ptrs+120(%rip)
	.size GetSaveFileNameA, .-GetSaveFileNameA

	.align 4
	.type Arc,@function
	.globl Arc
	.hidden Arc
Arc:
	jmpq *.L__wine_spec_import_data_ptrs+136(%rip)
	.size Arc, .-Arc

	.align 4
	.type BitBlt,@function
	.globl BitBlt
	.hidden BitBlt
BitBlt:
	jmpq *.L__wine_spec_import_data_ptrs+144(%rip)
	.size BitBlt, .-BitBlt

	.align 4
	.type CombineRgn,@function
	.globl CombineRgn
	.hidden CombineRgn
CombineRgn:
	jmpq *.L__wine_spec_import_data_ptrs+152(%rip)
	.size CombineRgn, .-CombineRgn

	.align 4
	.type CreateBitmap,@function
	.globl CreateBitmap
	.hidden CreateBitmap
CreateBitmap:
	jmpq *.L__wine_spec_import_data_ptrs+160(%rip)
	.size CreateBitmap, .-CreateBitmap

	.align 4
	.type CreateBrushIndirect,@function
	.globl CreateBrushIndirect
	.hidden CreateBrushIndirect
CreateBrushIndirect:
	jmpq *.L__wine_spec_import_data_ptrs+168(%rip)
	.size CreateBrushIndirect, .-CreateBrushIndirect

	.align 4
	.type CreateCompatibleBitmap,@function
	.globl CreateCompatibleBitmap
	.hidden CreateCompatibleBitmap
CreateCompatibleBitmap:
	jmpq *.L__wine_spec_import_data_ptrs+176(%rip)
	.size CreateCompatibleBitmap, .-CreateCompatibleBitmap

	.align 4
	.type CreateCompatibleDC,@function
	.globl CreateCompatibleDC
	.hidden CreateCompatibleDC
CreateCompatibleDC:
	jmpq *.L__wine_spec_import_data_ptrs+184(%rip)
	.size CreateCompatibleDC, .-CreateCompatibleDC

	.align 4
	.type CreateDCA,@function
	.globl CreateDCA
	.hidden CreateDCA
CreateDCA:
	jmpq *.L__wine_spec_import_data_ptrs+192(%rip)
	.size CreateDCA, .-CreateDCA

	.align 4
	.type CreateDIBitmap,@function
	.globl CreateDIBitmap
	.hidden CreateDIBitmap
CreateDIBitmap:
	jmpq *.L__wine_spec_import_data_ptrs+200(%rip)
	.size CreateDIBitmap, .-CreateDIBitmap

	.align 4
	.type CreateFontIndirectA,@function
	.globl CreateFontIndirectA
	.hidden CreateFontIndirectA
CreateFontIndirectA:
	jmpq *.L__wine_spec_import_data_ptrs+208(%rip)
	.size CreateFontIndirectA, .-CreateFontIndirectA

	.align 4
	.type CreatePatternBrush,@function
	.globl CreatePatternBrush
	.hidden CreatePatternBrush
CreatePatternBrush:
	jmpq *.L__wine_spec_import_data_ptrs+216(%rip)
	.size CreatePatternBrush, .-CreatePatternBrush

	.align 4
	.type CreatePen,@function
	.globl CreatePen
	.hidden CreatePen
CreatePen:
	jmpq *.L__wine_spec_import_data_ptrs+224(%rip)
	.size CreatePen, .-CreatePen

	.align 4
	.type CreateRectRgn,@function
	.globl CreateRectRgn
	.hidden CreateRectRgn
CreateRectRgn:
	jmpq *.L__wine_spec_import_data_ptrs+232(%rip)
	.size CreateRectRgn, .-CreateRectRgn

	.align 4
	.type CreateRectRgnIndirect,@function
	.globl CreateRectRgnIndirect
	.hidden CreateRectRgnIndirect
CreateRectRgnIndirect:
	jmpq *.L__wine_spec_import_data_ptrs+240(%rip)
	.size CreateRectRgnIndirect, .-CreateRectRgnIndirect

	.align 4
	.type CreateSolidBrush,@function
	.globl CreateSolidBrush
	.hidden CreateSolidBrush
CreateSolidBrush:
	jmpq *.L__wine_spec_import_data_ptrs+248(%rip)
	.size CreateSolidBrush, .-CreateSolidBrush

	.align 4
	.type DeleteDC,@function
	.globl DeleteDC
	.hidden DeleteDC
DeleteDC:
	jmpq *.L__wine_spec_import_data_ptrs+256(%rip)
	.size DeleteDC, .-DeleteDC

	.align 4
	.type DeleteObject,@function
	.globl DeleteObject
	.hidden DeleteObject
DeleteObject:
	jmpq *.L__wine_spec_import_data_ptrs+264(%rip)
	.size DeleteObject, .-DeleteObject

	.align 4
	.type Ellipse,@function
	.globl Ellipse
	.hidden Ellipse
Ellipse:
	jmpq *.L__wine_spec_import_data_ptrs+272(%rip)
	.size Ellipse, .-Ellipse

	.align 4
	.type EnumFontFamiliesA,@function
	.globl EnumFontFamiliesA
	.hidden EnumFontFamiliesA
EnumFontFamiliesA:
	jmpq *.L__wine_spec_import_data_ptrs+280(%rip)
	.size EnumFontFamiliesA, .-EnumFontFamiliesA

	.align 4
	.type ExcludeClipRect,@function
	.globl ExcludeClipRect
	.hidden ExcludeClipRect
ExcludeClipRect:
	jmpq *.L__wine_spec_import_data_ptrs+288(%rip)
	.size ExcludeClipRect, .-ExcludeClipRect

	.align 4
	.type ExtCreatePen,@function
	.globl ExtCreatePen
	.hidden ExtCreatePen
ExtCreatePen:
	jmpq *.L__wine_spec_import_data_ptrs+296(%rip)
	.size ExtCreatePen, .-ExtCreatePen

	.align 4
	.type ExtTextOutA,@function
	.globl ExtTextOutA
	.hidden ExtTextOutA
ExtTextOutA:
	jmpq *.L__wine_spec_import_data_ptrs+304(%rip)
	.size ExtTextOutA, .-ExtTextOutA

	.align 4
	.type ExtTextOutW,@function
	.globl ExtTextOutW
	.hidden ExtTextOutW
ExtTextOutW:
	jmpq *.L__wine_spec_import_data_ptrs+312(%rip)
	.size ExtTextOutW, .-ExtTextOutW

	.align 4
	.type FillRgn,@function
	.globl FillRgn
	.hidden FillRgn
FillRgn:
	jmpq *.L__wine_spec_import_data_ptrs+320(%rip)
	.size FillRgn, .-FillRgn

	.align 4
	.type GetBkColor,@function
	.globl GetBkColor
	.hidden GetBkColor
GetBkColor:
	jmpq *.L__wine_spec_import_data_ptrs+328(%rip)
	.size GetBkColor, .-GetBkColor

	.align 4
	.type GetClipBox,@function
	.globl GetClipBox
	.hidden GetClipBox
GetClipBox:
	jmpq *.L__wine_spec_import_data_ptrs+336(%rip)
	.size GetClipBox, .-GetClipBox

	.align 4
	.type GetClipRgn,@function
	.globl GetClipRgn
	.hidden GetClipRgn
GetClipRgn:
	jmpq *.L__wine_spec_import_data_ptrs+344(%rip)
	.size GetClipRgn, .-GetClipRgn

	.align 4
	.type GetCurrentObject,@function
	.globl GetCurrentObject
	.hidden GetCurrentObject
GetCurrentObject:
	jmpq *.L__wine_spec_import_data_ptrs+352(%rip)
	.size GetCurrentObject, .-GetCurrentObject

	.align 4
	.type GetDeviceCaps,@function
	.globl GetDeviceCaps
	.hidden GetDeviceCaps
GetDeviceCaps:
	jmpq *.L__wine_spec_import_data_ptrs+360(%rip)
	.size GetDeviceCaps, .-GetDeviceCaps

	.align 4
	.type GetDIBits,@function
	.globl GetDIBits
	.hidden GetDIBits
GetDIBits:
	jmpq *.L__wine_spec_import_data_ptrs+368(%rip)
	.size GetDIBits, .-GetDIBits

	.align 4
	.type GetMapMode,@function
	.globl GetMapMode
	.hidden GetMapMode
GetMapMode:
	jmpq *.L__wine_spec_import_data_ptrs+376(%rip)
	.size GetMapMode, .-GetMapMode

	.align 4
	.type GetObjectA,@function
	.globl GetObjectA
	.hidden GetObjectA
GetObjectA:
	jmpq *.L__wine_spec_import_data_ptrs+384(%rip)
	.size GetObjectA, .-GetObjectA

	.align 4
	.type GetStockObject,@function
	.globl GetStockObject
	.hidden GetStockObject
GetStockObject:
	jmpq *.L__wine_spec_import_data_ptrs+392(%rip)
	.size GetStockObject, .-GetStockObject

	.align 4
	.type GetTextColor,@function
	.globl GetTextColor
	.hidden GetTextColor
GetTextColor:
	jmpq *.L__wine_spec_import_data_ptrs+400(%rip)
	.size GetTextColor, .-GetTextColor

	.align 4
	.type GetTextExtentPoint32A,@function
	.globl GetTextExtentPoint32A
	.hidden GetTextExtentPoint32A
GetTextExtentPoint32A:
	jmpq *.L__wine_spec_import_data_ptrs+408(%rip)
	.size GetTextExtentPoint32A, .-GetTextExtentPoint32A

	.align 4
	.type GetTextMetricsA,@function
	.globl GetTextMetricsA
	.hidden GetTextMetricsA
GetTextMetricsA:
	jmpq *.L__wine_spec_import_data_ptrs+416(%rip)
	.size GetTextMetricsA, .-GetTextMetricsA

	.align 4
	.type IntersectClipRect,@function
	.globl IntersectClipRect
	.hidden IntersectClipRect
IntersectClipRect:
	jmpq *.L__wine_spec_import_data_ptrs+424(%rip)
	.size IntersectClipRect, .-IntersectClipRect

	.align 4
	.type LineTo,@function
	.globl LineTo
	.hidden LineTo
LineTo:
	jmpq *.L__wine_spec_import_data_ptrs+432(%rip)
	.size LineTo, .-LineTo

	.align 4
	.type ModifyWorldTransform,@function
	.globl ModifyWorldTransform
	.hidden ModifyWorldTransform
ModifyWorldTransform:
	jmpq *.L__wine_spec_import_data_ptrs+440(%rip)
	.size ModifyWorldTransform, .-ModifyWorldTransform

	.align 4
	.type MoveToEx,@function
	.globl MoveToEx
	.hidden MoveToEx
MoveToEx:
	jmpq *.L__wine_spec_import_data_ptrs+448(%rip)
	.size MoveToEx, .-MoveToEx

	.align 4
	.type PatBlt,@function
	.globl PatBlt
	.hidden PatBlt
PatBlt:
	jmpq *.L__wine_spec_import_data_ptrs+456(%rip)
	.size PatBlt, .-PatBlt

	.align 4
	.type Pie,@function
	.globl Pie
	.hidden Pie
Pie:
	jmpq *.L__wine_spec_import_data_ptrs+464(%rip)
	.size Pie, .-Pie

	.align 4
	.type Polygon,@function
	.globl Polygon
	.hidden Polygon
Polygon:
	jmpq *.L__wine_spec_import_data_ptrs+472(%rip)
	.size Polygon, .-Polygon

	.align 4
	.type Polyline,@function
	.globl Polyline
	.hidden Polyline
Polyline:
	jmpq *.L__wine_spec_import_data_ptrs+480(%rip)
	.size Polyline, .-Polyline

	.align 4
	.type Rectangle,@function
	.globl Rectangle
	.hidden Rectangle
Rectangle:
	jmpq *.L__wine_spec_import_data_ptrs+488(%rip)
	.size Rectangle, .-Rectangle

	.align 4
	.type RestoreDC,@function
	.globl RestoreDC
	.hidden RestoreDC
RestoreDC:
	jmpq *.L__wine_spec_import_data_ptrs+496(%rip)
	.size RestoreDC, .-RestoreDC

	.align 4
	.type SaveDC,@function
	.globl SaveDC
	.hidden SaveDC
SaveDC:
	jmpq *.L__wine_spec_import_data_ptrs+504(%rip)
	.size SaveDC, .-SaveDC

	.align 4
	.type SelectClipRgn,@function
	.globl SelectClipRgn
	.hidden SelectClipRgn
SelectClipRgn:
	jmpq *.L__wine_spec_import_data_ptrs+512(%rip)
	.size SelectClipRgn, .-SelectClipRgn

	.align 4
	.type SelectObject,@function
	.globl SelectObject
	.hidden SelectObject
SelectObject:
	jmpq *.L__wine_spec_import_data_ptrs+520(%rip)
	.size SelectObject, .-SelectObject

	.align 4
	.type SetBkColor,@function
	.globl SetBkColor
	.hidden SetBkColor
SetBkColor:
	jmpq *.L__wine_spec_import_data_ptrs+528(%rip)
	.size SetBkColor, .-SetBkColor

	.align 4
	.type SetBkMode,@function
	.globl SetBkMode
	.hidden SetBkMode
SetBkMode:
	jmpq *.L__wine_spec_import_data_ptrs+536(%rip)
	.size SetBkMode, .-SetBkMode

	.align 4
	.type SetBrushOrgEx,@function
	.globl SetBrushOrgEx
	.hidden SetBrushOrgEx
SetBrushOrgEx:
	jmpq *.L__wine_spec_import_data_ptrs+544(%rip)
	.size SetBrushOrgEx, .-SetBrushOrgEx

	.align 4
	.type SetGraphicsMode,@function
	.globl SetGraphicsMode
	.hidden SetGraphicsMode
SetGraphicsMode:
	jmpq *.L__wine_spec_import_data_ptrs+552(%rip)
	.size SetGraphicsMode, .-SetGraphicsMode

	.align 4
	.type SetMapMode,@function
	.globl SetMapMode
	.hidden SetMapMode
SetMapMode:
	jmpq *.L__wine_spec_import_data_ptrs+560(%rip)
	.size SetMapMode, .-SetMapMode

	.align 4
	.type SetPixelV,@function
	.globl SetPixelV
	.hidden SetPixelV
SetPixelV:
	jmpq *.L__wine_spec_import_data_ptrs+568(%rip)
	.size SetPixelV, .-SetPixelV

	.align 4
	.type SetPolyFillMode,@function
	.globl SetPolyFillMode
	.hidden SetPolyFillMode
SetPolyFillMode:
	jmpq *.L__wine_spec_import_data_ptrs+576(%rip)
	.size SetPolyFillMode, .-SetPolyFillMode

	.align 4
	.type SetRectRgn,@function
	.globl SetRectRgn
	.hidden SetRectRgn
SetRectRgn:
	jmpq *.L__wine_spec_import_data_ptrs+584(%rip)
	.size SetRectRgn, .-SetRectRgn

	.align 4
	.type SetROP2,@function
	.globl SetROP2
	.hidden SetROP2
SetROP2:
	jmpq *.L__wine_spec_import_data_ptrs+592(%rip)
	.size SetROP2, .-SetROP2

	.align 4
	.type SetStretchBltMode,@function
	.globl SetStretchBltMode
	.hidden SetStretchBltMode
SetStretchBltMode:
	jmpq *.L__wine_spec_import_data_ptrs+600(%rip)
	.size SetStretchBltMode, .-SetStretchBltMode

	.align 4
	.type SetTextAlign,@function
	.globl SetTextAlign
	.hidden SetTextAlign
SetTextAlign:
	jmpq *.L__wine_spec_import_data_ptrs+608(%rip)
	.size SetTextAlign, .-SetTextAlign

	.align 4
	.type SetTextColor,@function
	.globl SetTextColor
	.hidden SetTextColor
SetTextColor:
	jmpq *.L__wine_spec_import_data_ptrs+616(%rip)
	.size SetTextColor, .-SetTextColor

	.align 4
	.type SetViewportExtEx,@function
	.globl SetViewportExtEx
	.hidden SetViewportExtEx
SetViewportExtEx:
	jmpq *.L__wine_spec_import_data_ptrs+624(%rip)
	.size SetViewportExtEx, .-SetViewportExtEx

	.align 4
	.type SetViewportOrgEx,@function
	.globl SetViewportOrgEx
	.hidden SetViewportOrgEx
SetViewportOrgEx:
	jmpq *.L__wine_spec_import_data_ptrs+632(%rip)
	.size SetViewportOrgEx, .-SetViewportOrgEx

	.align 4
	.type SetWindowExtEx,@function
	.globl SetWindowExtEx
	.hidden SetWindowExtEx
SetWindowExtEx:
	jmpq *.L__wine_spec_import_data_ptrs+640(%rip)
	.size SetWindowExtEx, .-SetWindowExtEx

	.align 4
	.type StretchBlt,@function
	.globl StretchBlt
	.hidden StretchBlt
StretchBlt:
	jmpq *.L__wine_spec_import_data_ptrs+648(%rip)
	.size StretchBlt, .-StretchBlt

	.align 4
	.type TextOutA,@function
	.globl TextOutA
	.hidden TextOutA
TextOutA:
	jmpq *.L__wine_spec_import_data_ptrs+656(%rip)
	.size TextOutA, .-TextOutA

	.align 4
	.type GetRgnBox,@function
	.globl GetRgnBox
	.hidden GetRgnBox
GetRgnBox:
	jmpq *.L__wine_spec_import_data_ptrs+664(%rip)
	.size GetRgnBox, .-GetRgnBox

	.align 4
	.type SetDIBits,@function
	.globl SetDIBits
	.hidden SetDIBits
SetDIBits:
	jmpq *.L__wine_spec_import_data_ptrs+672(%rip)
	.size SetDIBits, .-SetDIBits

	.align 4
	.type RegCloseKey,@function
	.globl RegCloseKey
	.hidden RegCloseKey
RegCloseKey:
	jmpq *.L__wine_spec_import_data_ptrs+688(%rip)
	.size RegCloseKey, .-RegCloseKey

	.align 4
	.type RegCreateKeyExW,@function
	.globl RegCreateKeyExW
	.hidden RegCreateKeyExW
RegCreateKeyExW:
	jmpq *.L__wine_spec_import_data_ptrs+696(%rip)
	.size RegCreateKeyExW, .-RegCreateKeyExW

	.align 4
	.type RegOpenKeyExW,@function
	.globl RegOpenKeyExW
	.hidden RegOpenKeyExW
RegOpenKeyExW:
	jmpq *.L__wine_spec_import_data_ptrs+704(%rip)
	.size RegOpenKeyExW, .-RegOpenKeyExW

	.align 4
	.type RegQueryValueExW,@function
	.globl RegQueryValueExW
	.hidden RegQueryValueExW
RegQueryValueExW:
	jmpq *.L__wine_spec_import_data_ptrs+712(%rip)
	.size RegQueryValueExW, .-RegQueryValueExW

	.align 4
	.type RegSetValueExW,@function
	.globl RegSetValueExW
	.hidden RegSetValueExW
RegSetValueExW:
	jmpq *.L__wine_spec_import_data_ptrs+720(%rip)
	.size RegSetValueExW, .-RegSetValueExW

	.align 4
	.type AdjustWindowRect,@function
	.globl AdjustWindowRect
	.hidden AdjustWindowRect
AdjustWindowRect:
	jmpq *.L__wine_spec_import_data_ptrs+736(%rip)
	.size AdjustWindowRect, .-AdjustWindowRect

	.align 4
	.type AdjustWindowRectEx,@function
	.globl AdjustWindowRectEx
	.hidden AdjustWindowRectEx
AdjustWindowRectEx:
	jmpq *.L__wine_spec_import_data_ptrs+744(%rip)
	.size AdjustWindowRectEx, .-AdjustWindowRectEx

	.align 4
	.type AppendMenuA,@function
	.globl AppendMenuA
	.hidden AppendMenuA
AppendMenuA:
	jmpq *.L__wine_spec_import_data_ptrs+752(%rip)
	.size AppendMenuA, .-AppendMenuA

	.align 4
	.type BeginPaint,@function
	.globl BeginPaint
	.hidden BeginPaint
BeginPaint:
	jmpq *.L__wine_spec_import_data_ptrs+760(%rip)
	.size BeginPaint, .-BeginPaint

	.align 4
	.type CallWindowProcA,@function
	.globl CallWindowProcA
	.hidden CallWindowProcA
CallWindowProcA:
	jmpq *.L__wine_spec_import_data_ptrs+768(%rip)
	.size CallWindowProcA, .-CallWindowProcA

	.align 4
	.type ClientToScreen,@function
	.globl ClientToScreen
	.hidden ClientToScreen
ClientToScreen:
	jmpq *.L__wine_spec_import_data_ptrs+776(%rip)
	.size ClientToScreen, .-ClientToScreen

	.align 4
	.type CreateAcceleratorTableA,@function
	.globl CreateAcceleratorTableA
	.hidden CreateAcceleratorTableA
CreateAcceleratorTableA:
	jmpq *.L__wine_spec_import_data_ptrs+784(%rip)
	.size CreateAcceleratorTableA, .-CreateAcceleratorTableA

	.align 4
	.type CreateDialogIndirectParamA,@function
	.globl CreateDialogIndirectParamA
	.hidden CreateDialogIndirectParamA
CreateDialogIndirectParamA:
	jmpq *.L__wine_spec_import_data_ptrs+792(%rip)
	.size CreateDialogIndirectParamA, .-CreateDialogIndirectParamA

	.align 4
	.type CreateMenu,@function
	.globl CreateMenu
	.hidden CreateMenu
CreateMenu:
	jmpq *.L__wine_spec_import_data_ptrs+800(%rip)
	.size CreateMenu, .-CreateMenu

	.align 4
	.type CreatePopupMenu,@function
	.globl CreatePopupMenu
	.hidden CreatePopupMenu
CreatePopupMenu:
	jmpq *.L__wine_spec_import_data_ptrs+808(%rip)
	.size CreatePopupMenu, .-CreatePopupMenu

	.align 4
	.type CreateWindowExA,@function
	.globl CreateWindowExA
	.hidden CreateWindowExA
CreateWindowExA:
	jmpq *.L__wine_spec_import_data_ptrs+816(%rip)
	.size CreateWindowExA, .-CreateWindowExA

	.align 4
	.type CreateWindowExW,@function
	.globl CreateWindowExW
	.hidden CreateWindowExW
CreateWindowExW:
	jmpq *.L__wine_spec_import_data_ptrs+824(%rip)
	.size CreateWindowExW, .-CreateWindowExW

	.align 4
	.type DefDlgProcA,@function
	.globl DefDlgProcA
	.hidden DefDlgProcA
DefDlgProcA:
	jmpq *.L__wine_spec_import_data_ptrs+832(%rip)
	.size DefDlgProcA, .-DefDlgProcA

	.align 4
	.type DefFrameProcW,@function
	.globl DefFrameProcW
	.hidden DefFrameProcW
DefFrameProcW:
	jmpq *.L__wine_spec_import_data_ptrs+840(%rip)
	.size DefFrameProcW, .-DefFrameProcW

	.align 4
	.type DefMDIChildProcA,@function
	.globl DefMDIChildProcA
	.hidden DefMDIChildProcA
DefMDIChildProcA:
	jmpq *.L__wine_spec_import_data_ptrs+848(%rip)
	.size DefMDIChildProcA, .-DefMDIChildProcA

	.align 4
	.type DefWindowProcA,@function
	.globl DefWindowProcA
	.hidden DefWindowProcA
DefWindowProcA:
	jmpq *.L__wine_spec_import_data_ptrs+856(%rip)
	.size DefWindowProcA, .-DefWindowProcA

	.align 4
	.type DefWindowProcW,@function
	.globl DefWindowProcW
	.hidden DefWindowProcW
DefWindowProcW:
	jmpq *.L__wine_spec_import_data_ptrs+864(%rip)
	.size DefWindowProcW, .-DefWindowProcW

	.align 4
	.type DeleteMenu,@function
	.globl DeleteMenu
	.hidden DeleteMenu
DeleteMenu:
	jmpq *.L__wine_spec_import_data_ptrs+872(%rip)
	.size DeleteMenu, .-DeleteMenu

	.align 4
	.type DestroyAcceleratorTable,@function
	.globl DestroyAcceleratorTable
	.hidden DestroyAcceleratorTable
DestroyAcceleratorTable:
	jmpq *.L__wine_spec_import_data_ptrs+880(%rip)
	.size DestroyAcceleratorTable, .-DestroyAcceleratorTable

	.align 4
	.type DestroyWindow,@function
	.globl DestroyWindow
	.hidden DestroyWindow
DestroyWindow:
	jmpq *.L__wine_spec_import_data_ptrs+888(%rip)
	.size DestroyWindow, .-DestroyWindow

	.align 4
	.type DialogBoxIndirectParamA,@function
	.globl DialogBoxIndirectParamA
	.hidden DialogBoxIndirectParamA
DialogBoxIndirectParamA:
	jmpq *.L__wine_spec_import_data_ptrs+896(%rip)
	.size DialogBoxIndirectParamA, .-DialogBoxIndirectParamA

	.align 4
	.type DispatchMessageA,@function
	.globl DispatchMessageA
	.hidden DispatchMessageA
DispatchMessageA:
	jmpq *.L__wine_spec_import_data_ptrs+904(%rip)
	.size DispatchMessageA, .-DispatchMessageA

	.align 4
	.type DrawEdge,@function
	.globl DrawEdge
	.hidden DrawEdge
DrawEdge:
	jmpq *.L__wine_spec_import_data_ptrs+912(%rip)
	.size DrawEdge, .-DrawEdge

	.align 4
	.type DrawFocusRect,@function
	.globl DrawFocusRect
	.hidden DrawFocusRect
DrawFocusRect:
	jmpq *.L__wine_spec_import_data_ptrs+920(%rip)
	.size DrawFocusRect, .-DrawFocusRect

	.align 4
	.type DrawFrameControl,@function
	.globl DrawFrameControl
	.hidden DrawFrameControl
DrawFrameControl:
	jmpq *.L__wine_spec_import_data_ptrs+928(%rip)
	.size DrawFrameControl, .-DrawFrameControl

	.align 4
	.type DrawMenuBar,@function
	.globl DrawMenuBar
	.hidden DrawMenuBar
DrawMenuBar:
	jmpq *.L__wine_spec_import_data_ptrs+936(%rip)
	.size DrawMenuBar, .-DrawMenuBar

	.align 4
	.type DrawTextA,@function
	.globl DrawTextA
	.hidden DrawTextA
DrawTextA:
	jmpq *.L__wine_spec_import_data_ptrs+944(%rip)
	.size DrawTextA, .-DrawTextA

	.align 4
	.type DrawTextW,@function
	.globl DrawTextW
	.hidden DrawTextW
DrawTextW:
	jmpq *.L__wine_spec_import_data_ptrs+952(%rip)
	.size DrawTextW, .-DrawTextW

	.align 4
	.type EnableMenuItem,@function
	.globl EnableMenuItem
	.hidden EnableMenuItem
EnableMenuItem:
	jmpq *.L__wine_spec_import_data_ptrs+960(%rip)
	.size EnableMenuItem, .-EnableMenuItem

	.align 4
	.type EnableWindow,@function
	.globl EnableWindow
	.hidden EnableWindow
EnableWindow:
	jmpq *.L__wine_spec_import_data_ptrs+968(%rip)
	.size EnableWindow, .-EnableWindow

	.align 4
	.type EndDialog,@function
	.globl EndDialog
	.hidden EndDialog
EndDialog:
	jmpq *.L__wine_spec_import_data_ptrs+976(%rip)
	.size EndDialog, .-EndDialog

	.align 4
	.type EndPaint,@function
	.globl EndPaint
	.hidden EndPaint
EndPaint:
	jmpq *.L__wine_spec_import_data_ptrs+984(%rip)
	.size EndPaint, .-EndPaint

	.align 4
	.type FillRect,@function
	.globl FillRect
	.hidden FillRect
FillRect:
	jmpq *.L__wine_spec_import_data_ptrs+992(%rip)
	.size FillRect, .-FillRect

	.align 4
	.type GetAsyncKeyState,@function
	.globl GetAsyncKeyState
	.hidden GetAsyncKeyState
GetAsyncKeyState:
	jmpq *.L__wine_spec_import_data_ptrs+1000(%rip)
	.size GetAsyncKeyState, .-GetAsyncKeyState

	.align 4
	.type GetCapture,@function
	.globl GetCapture
	.hidden GetCapture
GetCapture:
	jmpq *.L__wine_spec_import_data_ptrs+1008(%rip)
	.size GetCapture, .-GetCapture

	.align 4
	.type GetClassInfoW,@function
	.globl GetClassInfoW
	.hidden GetClassInfoW
GetClassInfoW:
	jmpq *.L__wine_spec_import_data_ptrs+1016(%rip)
	.size GetClassInfoW, .-GetClassInfoW

	.align 4
	.type GetClassLongPtrA,@function
	.globl GetClassLongPtrA
	.hidden GetClassLongPtrA
GetClassLongPtrA:
	jmpq *.L__wine_spec_import_data_ptrs+1024(%rip)
	.size GetClassLongPtrA, .-GetClassLongPtrA

	.align 4
	.type GetClassNameA,@function
	.globl GetClassNameA
	.hidden GetClassNameA
GetClassNameA:
	jmpq *.L__wine_spec_import_data_ptrs+1032(%rip)
	.size GetClassNameA, .-GetClassNameA

	.align 4
	.type GetClassNameW,@function
	.globl GetClassNameW
	.hidden GetClassNameW
GetClassNameW:
	jmpq *.L__wine_spec_import_data_ptrs+1040(%rip)
	.size GetClassNameW, .-GetClassNameW

	.align 4
	.type GetClientRect,@function
	.globl GetClientRect
	.hidden GetClientRect
GetClientRect:
	jmpq *.L__wine_spec_import_data_ptrs+1048(%rip)
	.size GetClientRect, .-GetClientRect

	.align 4
	.type GetCursorPos,@function
	.globl GetCursorPos
	.hidden GetCursorPos
GetCursorPos:
	jmpq *.L__wine_spec_import_data_ptrs+1056(%rip)
	.size GetCursorPos, .-GetCursorPos

	.align 4
	.type GetDC,@function
	.globl GetDC
	.hidden GetDC
GetDC:
	jmpq *.L__wine_spec_import_data_ptrs+1064(%rip)
	.size GetDC, .-GetDC

	.align 4
	.type GetDCEx,@function
	.globl GetDCEx
	.hidden GetDCEx
GetDCEx:
	jmpq *.L__wine_spec_import_data_ptrs+1072(%rip)
	.size GetDCEx, .-GetDCEx

	.align 4
	.type GetDesktopWindow,@function
	.globl GetDesktopWindow
	.hidden GetDesktopWindow
GetDesktopWindow:
	jmpq *.L__wine_spec_import_data_ptrs+1080(%rip)
	.size GetDesktopWindow, .-GetDesktopWindow

	.align 4
	.type GetDialogBaseUnits,@function
	.globl GetDialogBaseUnits
	.hidden GetDialogBaseUnits
GetDialogBaseUnits:
	jmpq *.L__wine_spec_import_data_ptrs+1088(%rip)
	.size GetDialogBaseUnits, .-GetDialogBaseUnits

	.align 4
	.type GetKeyState,@function
	.globl GetKeyState
	.hidden GetKeyState
GetKeyState:
	jmpq *.L__wine_spec_import_data_ptrs+1096(%rip)
	.size GetKeyState, .-GetKeyState

	.align 4
	.type GetMenu,@function
	.globl GetMenu
	.hidden GetMenu
GetMenu:
	jmpq *.L__wine_spec_import_data_ptrs+1104(%rip)
	.size GetMenu, .-GetMenu

	.align 4
	.type GetMenuCheckMarkDimensions,@function
	.globl GetMenuCheckMarkDimensions
	.hidden GetMenuCheckMarkDimensions
GetMenuCheckMarkDimensions:
	jmpq *.L__wine_spec_import_data_ptrs+1112(%rip)
	.size GetMenuCheckMarkDimensions, .-GetMenuCheckMarkDimensions

	.align 4
	.type GetMenuItemInfoA,@function
	.globl GetMenuItemInfoA
	.hidden GetMenuItemInfoA
GetMenuItemInfoA:
	jmpq *.L__wine_spec_import_data_ptrs+1120(%rip)
	.size GetMenuItemInfoA, .-GetMenuItemInfoA

	.align 4
	.type GetMessageA,@function
	.globl GetMessageA
	.hidden GetMessageA
GetMessageA:
	jmpq *.L__wine_spec_import_data_ptrs+1128(%rip)
	.size GetMessageA, .-GetMessageA

	.align 4
	.type GetParent,@function
	.globl GetParent
	.hidden GetParent
GetParent:
	jmpq *.L__wine_spec_import_data_ptrs+1136(%rip)
	.size GetParent, .-GetParent

	.align 4
	.type GetSysColor,@function
	.globl GetSysColor
	.hidden GetSysColor
GetSysColor:
	jmpq *.L__wine_spec_import_data_ptrs+1144(%rip)
	.size GetSysColor, .-GetSysColor

	.align 4
	.type GetSysColorBrush,@function
	.globl GetSysColorBrush
	.hidden GetSysColorBrush
GetSysColorBrush:
	jmpq *.L__wine_spec_import_data_ptrs+1152(%rip)
	.size GetSysColorBrush, .-GetSysColorBrush

	.align 4
	.type GetSystemMenu,@function
	.globl GetSystemMenu
	.hidden GetSystemMenu
GetSystemMenu:
	jmpq *.L__wine_spec_import_data_ptrs+1160(%rip)
	.size GetSystemMenu, .-GetSystemMenu

	.align 4
	.type GetSystemMetrics,@function
	.globl GetSystemMetrics
	.hidden GetSystemMetrics
GetSystemMetrics:
	jmpq *.L__wine_spec_import_data_ptrs+1168(%rip)
	.size GetSystemMetrics, .-GetSystemMetrics

	.align 4
	.type GetTopWindow,@function
	.globl GetTopWindow
	.hidden GetTopWindow
GetTopWindow:
	jmpq *.L__wine_spec_import_data_ptrs+1176(%rip)
	.size GetTopWindow, .-GetTopWindow

	.align 4
	.type GetWindow,@function
	.globl GetWindow
	.hidden GetWindow
GetWindow:
	jmpq *.L__wine_spec_import_data_ptrs+1184(%rip)
	.size GetWindow, .-GetWindow

	.align 4
	.type GetWindowDC,@function
	.globl GetWindowDC
	.hidden GetWindowDC
GetWindowDC:
	jmpq *.L__wine_spec_import_data_ptrs+1192(%rip)
	.size GetWindowDC, .-GetWindowDC

	.align 4
	.type GetWindowLongA,@function
	.globl GetWindowLongA
	.hidden GetWindowLongA
GetWindowLongA:
	jmpq *.L__wine_spec_import_data_ptrs+1200(%rip)
	.size GetWindowLongA, .-GetWindowLongA

	.align 4
	.type GetWindowLongPtrA,@function
	.globl GetWindowLongPtrA
	.hidden GetWindowLongPtrA
GetWindowLongPtrA:
	jmpq *.L__wine_spec_import_data_ptrs+1208(%rip)
	.size GetWindowLongPtrA, .-GetWindowLongPtrA

	.align 4
	.type GetWindowPlacement,@function
	.globl GetWindowPlacement
	.hidden GetWindowPlacement
GetWindowPlacement:
	jmpq *.L__wine_spec_import_data_ptrs+1216(%rip)
	.size GetWindowPlacement, .-GetWindowPlacement

	.align 4
	.type GetWindowRect,@function
	.globl GetWindowRect
	.hidden GetWindowRect
GetWindowRect:
	jmpq *.L__wine_spec_import_data_ptrs+1224(%rip)
	.size GetWindowRect, .-GetWindowRect

	.align 4
	.type GetWindowTextA,@function
	.globl GetWindowTextA
	.hidden GetWindowTextA
GetWindowTextA:
	jmpq *.L__wine_spec_import_data_ptrs+1232(%rip)
	.size GetWindowTextA, .-GetWindowTextA

	.align 4
	.type GetWindowTextLengthA,@function
	.globl GetWindowTextLengthA
	.hidden GetWindowTextLengthA
GetWindowTextLengthA:
	jmpq *.L__wine_spec_import_data_ptrs+1240(%rip)
	.size GetWindowTextLengthA, .-GetWindowTextLengthA

	.align 4
	.type GetWindowTextLengthW,@function
	.globl GetWindowTextLengthW
	.hidden GetWindowTextLengthW
GetWindowTextLengthW:
	jmpq *.L__wine_spec_import_data_ptrs+1248(%rip)
	.size GetWindowTextLengthW, .-GetWindowTextLengthW

	.align 4
	.type GetWindowTextW,@function
	.globl GetWindowTextW
	.hidden GetWindowTextW
GetWindowTextW:
	jmpq *.L__wine_spec_import_data_ptrs+1256(%rip)
	.size GetWindowTextW, .-GetWindowTextW

	.align 4
	.type InflateRect,@function
	.globl InflateRect
	.hidden InflateRect
InflateRect:
	jmpq *.L__wine_spec_import_data_ptrs+1264(%rip)
	.size InflateRect, .-InflateRect

	.align 4
	.type InsertMenuA,@function
	.globl InsertMenuA
	.hidden InsertMenuA
InsertMenuA:
	jmpq *.L__wine_spec_import_data_ptrs+1272(%rip)
	.size InsertMenuA, .-InsertMenuA

	.align 4
	.type InsertMenuItemA,@function
	.globl InsertMenuItemA
	.hidden InsertMenuItemA
InsertMenuItemA:
	jmpq *.L__wine_spec_import_data_ptrs+1280(%rip)
	.size InsertMenuItemA, .-InsertMenuItemA

	.align 4
	.type IntersectRect,@function
	.globl IntersectRect
	.hidden IntersectRect
IntersectRect:
	jmpq *.L__wine_spec_import_data_ptrs+1288(%rip)
	.size IntersectRect, .-IntersectRect

	.align 4
	.type InvalidateRect,@function
	.globl InvalidateRect
	.hidden InvalidateRect
InvalidateRect:
	jmpq *.L__wine_spec_import_data_ptrs+1296(%rip)
	.size InvalidateRect, .-InvalidateRect

	.align 4
	.type IsDialogMessageA,@function
	.globl IsDialogMessageA
	.hidden IsDialogMessageA
IsDialogMessageA:
	jmpq *.L__wine_spec_import_data_ptrs+1304(%rip)
	.size IsDialogMessageA, .-IsDialogMessageA

	.align 4
	.type IsIconic,@function
	.globl IsIconic
	.hidden IsIconic
IsIconic:
	jmpq *.L__wine_spec_import_data_ptrs+1312(%rip)
	.size IsIconic, .-IsIconic

	.align 4
	.type IsWindow,@function
	.globl IsWindow
	.hidden IsWindow
IsWindow:
	jmpq *.L__wine_spec_import_data_ptrs+1320(%rip)
	.size IsWindow, .-IsWindow

	.align 4
	.type IsWindowEnabled,@function
	.globl IsWindowEnabled
	.hidden IsWindowEnabled
IsWindowEnabled:
	jmpq *.L__wine_spec_import_data_ptrs+1328(%rip)
	.size IsWindowEnabled, .-IsWindowEnabled

	.align 4
	.type IsWindowVisible,@function
	.globl IsWindowVisible
	.hidden IsWindowVisible
IsWindowVisible:
	jmpq *.L__wine_spec_import_data_ptrs+1336(%rip)
	.size IsWindowVisible, .-IsWindowVisible

	.align 4
	.type KillTimer,@function
	.globl KillTimer
	.hidden KillTimer
KillTimer:
	jmpq *.L__wine_spec_import_data_ptrs+1344(%rip)
	.size KillTimer, .-KillTimer

	.align 4
	.type LoadCursorA,@function
	.globl LoadCursorA
	.hidden LoadCursorA
LoadCursorA:
	jmpq *.L__wine_spec_import_data_ptrs+1352(%rip)
	.size LoadCursorA, .-LoadCursorA

	.align 4
	.type LoadIconA,@function
	.globl LoadIconA
	.hidden LoadIconA
LoadIconA:
	jmpq *.L__wine_spec_import_data_ptrs+1360(%rip)
	.size LoadIconA, .-LoadIconA

	.align 4
	.type LoadImageA,@function
	.globl LoadImageA
	.hidden LoadImageA
LoadImageA:
	jmpq *.L__wine_spec_import_data_ptrs+1368(%rip)
	.size LoadImageA, .-LoadImageA

	.align 4
	.type LockWindowUpdate,@function
	.globl LockWindowUpdate
	.hidden LockWindowUpdate
LockWindowUpdate:
	jmpq *.L__wine_spec_import_data_ptrs+1376(%rip)
	.size LockWindowUpdate, .-LockWindowUpdate

	.align 4
	.type MapWindowPoints,@function
	.globl MapWindowPoints
	.hidden MapWindowPoints
MapWindowPoints:
	jmpq *.L__wine_spec_import_data_ptrs+1384(%rip)
	.size MapWindowPoints, .-MapWindowPoints

	.align 4
	.type MessageBoxW,@function
	.globl MessageBoxW
	.hidden MessageBoxW
MessageBoxW:
	jmpq *.L__wine_spec_import_data_ptrs+1392(%rip)
	.size MessageBoxW, .-MessageBoxW

	.align 4
	.type MoveWindow,@function
	.globl MoveWindow
	.hidden MoveWindow
MoveWindow:
	jmpq *.L__wine_spec_import_data_ptrs+1400(%rip)
	.size MoveWindow, .-MoveWindow

	.align 4
	.type OffsetRect,@function
	.globl OffsetRect
	.hidden OffsetRect
OffsetRect:
	jmpq *.L__wine_spec_import_data_ptrs+1408(%rip)
	.size OffsetRect, .-OffsetRect

	.align 4
	.type PeekMessageA,@function
	.globl PeekMessageA
	.hidden PeekMessageA
PeekMessageA:
	jmpq *.L__wine_spec_import_data_ptrs+1416(%rip)
	.size PeekMessageA, .-PeekMessageA

	.align 4
	.type PostQuitMessage,@function
	.globl PostQuitMessage
	.hidden PostQuitMessage
PostQuitMessage:
	jmpq *.L__wine_spec_import_data_ptrs+1424(%rip)
	.size PostQuitMessage, .-PostQuitMessage

	.align 4
	.type PtInRect,@function
	.globl PtInRect
	.hidden PtInRect
PtInRect:
	jmpq *.L__wine_spec_import_data_ptrs+1432(%rip)
	.size PtInRect, .-PtInRect

	.align 4
	.type RegisterClassA,@function
	.globl RegisterClassA
	.hidden RegisterClassA
RegisterClassA:
	jmpq *.L__wine_spec_import_data_ptrs+1440(%rip)
	.size RegisterClassA, .-RegisterClassA

	.align 4
	.type RegisterClassW,@function
	.globl RegisterClassW
	.hidden RegisterClassW
RegisterClassW:
	jmpq *.L__wine_spec_import_data_ptrs+1448(%rip)
	.size RegisterClassW, .-RegisterClassW

	.align 4
	.type ReleaseCapture,@function
	.globl ReleaseCapture
	.hidden ReleaseCapture
ReleaseCapture:
	jmpq *.L__wine_spec_import_data_ptrs+1456(%rip)
	.size ReleaseCapture, .-ReleaseCapture

	.align 4
	.type ReleaseDC,@function
	.globl ReleaseDC
	.hidden ReleaseDC
ReleaseDC:
	jmpq *.L__wine_spec_import_data_ptrs+1464(%rip)
	.size ReleaseDC, .-ReleaseDC

	.align 4
	.type ScreenToClient,@function
	.globl ScreenToClient
	.hidden ScreenToClient
ScreenToClient:
	jmpq *.L__wine_spec_import_data_ptrs+1472(%rip)
	.size ScreenToClient, .-ScreenToClient

	.align 4
	.type SendDlgItemMessageA,@function
	.globl SendDlgItemMessageA
	.hidden SendDlgItemMessageA
SendDlgItemMessageA:
	jmpq *.L__wine_spec_import_data_ptrs+1480(%rip)
	.size SendDlgItemMessageA, .-SendDlgItemMessageA

	.align 4
	.type SendMessageA,@function
	.globl SendMessageA
	.hidden SendMessageA
SendMessageA:
	jmpq *.L__wine_spec_import_data_ptrs+1488(%rip)
	.size SendMessageA, .-SendMessageA

	.align 4
	.type SendMessageW,@function
	.globl SendMessageW
	.hidden SendMessageW
SendMessageW:
	jmpq *.L__wine_spec_import_data_ptrs+1496(%rip)
	.size SendMessageW, .-SendMessageW

	.align 4
	.type PostMessageW,@function
	.globl PostMessageW
	.hidden PostMessageW
PostMessageW:
	jmpq *.L__wine_spec_import_data_ptrs+1504(%rip)
	.size PostMessageW, .-PostMessageW

	.align 4
	.type SetActiveWindow,@function
	.globl SetActiveWindow
	.hidden SetActiveWindow
SetActiveWindow:
	jmpq *.L__wine_spec_import_data_ptrs+1512(%rip)
	.size SetActiveWindow, .-SetActiveWindow

	.align 4
	.type SetCapture,@function
	.globl SetCapture
	.hidden SetCapture
SetCapture:
	jmpq *.L__wine_spec_import_data_ptrs+1520(%rip)
	.size SetCapture, .-SetCapture

	.align 4
	.type SetCursor,@function
	.globl SetCursor
	.hidden SetCursor
SetCursor:
	jmpq *.L__wine_spec_import_data_ptrs+1528(%rip)
	.size SetCursor, .-SetCursor

	.align 4
	.type SetFocus,@function
	.globl SetFocus
	.hidden SetFocus
SetFocus:
	jmpq *.L__wine_spec_import_data_ptrs+1536(%rip)
	.size SetFocus, .-SetFocus

	.align 4
	.type SetMenu,@function
	.globl SetMenu
	.hidden SetMenu
SetMenu:
	jmpq *.L__wine_spec_import_data_ptrs+1544(%rip)
	.size SetMenu, .-SetMenu

	.align 4
	.type SetMenuItemInfoA,@function
	.globl SetMenuItemInfoA
	.hidden SetMenuItemInfoA
SetMenuItemInfoA:
	jmpq *.L__wine_spec_import_data_ptrs+1552(%rip)
	.size SetMenuItemInfoA, .-SetMenuItemInfoA

	.align 4
	.type SetParent,@function
	.globl SetParent
	.hidden SetParent
SetParent:
	jmpq *.L__wine_spec_import_data_ptrs+1560(%rip)
	.size SetParent, .-SetParent

	.align 4
	.type SetRectEmpty,@function
	.globl SetRectEmpty
	.hidden SetRectEmpty
SetRectEmpty:
	jmpq *.L__wine_spec_import_data_ptrs+1568(%rip)
	.size SetRectEmpty, .-SetRectEmpty

	.align 4
	.type SetScrollInfo,@function
	.globl SetScrollInfo
	.hidden SetScrollInfo
SetScrollInfo:
	jmpq *.L__wine_spec_import_data_ptrs+1576(%rip)
	.size SetScrollInfo, .-SetScrollInfo

	.align 4
	.type SetScrollPos,@function
	.globl SetScrollPos
	.hidden SetScrollPos
SetScrollPos:
	jmpq *.L__wine_spec_import_data_ptrs+1584(%rip)
	.size SetScrollPos, .-SetScrollPos

	.align 4
	.type SetTimer,@function
	.globl SetTimer
	.hidden SetTimer
SetTimer:
	jmpq *.L__wine_spec_import_data_ptrs+1592(%rip)
	.size SetTimer, .-SetTimer

	.align 4
	.type SetWindowLongA,@function
	.globl SetWindowLongA
	.hidden SetWindowLongA
SetWindowLongA:
	jmpq *.L__wine_spec_import_data_ptrs+1600(%rip)
	.size SetWindowLongA, .-SetWindowLongA

	.align 4
	.type SetWindowLongPtrA,@function
	.globl SetWindowLongPtrA
	.hidden SetWindowLongPtrA
SetWindowLongPtrA:
	jmpq *.L__wine_spec_import_data_ptrs+1608(%rip)
	.size SetWindowLongPtrA, .-SetWindowLongPtrA

	.align 4
	.type SetWindowPos,@function
	.globl SetWindowPos
	.hidden SetWindowPos
SetWindowPos:
	jmpq *.L__wine_spec_import_data_ptrs+1616(%rip)
	.size SetWindowPos, .-SetWindowPos

	.align 4
	.type SetWindowTextA,@function
	.globl SetWindowTextA
	.hidden SetWindowTextA
SetWindowTextA:
	jmpq *.L__wine_spec_import_data_ptrs+1624(%rip)
	.size SetWindowTextA, .-SetWindowTextA

	.align 4
	.type SetWindowTextW,@function
	.globl SetWindowTextW
	.hidden SetWindowTextW
SetWindowTextW:
	jmpq *.L__wine_spec_import_data_ptrs+1632(%rip)
	.size SetWindowTextW, .-SetWindowTextW

	.align 4
	.type ShowWindow,@function
	.globl ShowWindow
	.hidden ShowWindow
ShowWindow:
	jmpq *.L__wine_spec_import_data_ptrs+1640(%rip)
	.size ShowWindow, .-ShowWindow

	.align 4
	.type SystemParametersInfoA,@function
	.globl SystemParametersInfoA
	.hidden SystemParametersInfoA
SystemParametersInfoA:
	jmpq *.L__wine_spec_import_data_ptrs+1648(%rip)
	.size SystemParametersInfoA, .-SystemParametersInfoA

	.align 4
	.type TrackPopupMenu,@function
	.globl TrackPopupMenu
	.hidden TrackPopupMenu
TrackPopupMenu:
	jmpq *.L__wine_spec_import_data_ptrs+1656(%rip)
	.size TrackPopupMenu, .-TrackPopupMenu

	.align 4
	.type TranslateAcceleratorA,@function
	.globl TranslateAcceleratorA
	.hidden TranslateAcceleratorA
TranslateAcceleratorA:
	jmpq *.L__wine_spec_import_data_ptrs+1664(%rip)
	.size TranslateAcceleratorA, .-TranslateAcceleratorA

	.align 4
	.type TranslateMDISysAccel,@function
	.globl TranslateMDISysAccel
	.hidden TranslateMDISysAccel
TranslateMDISysAccel:
	jmpq *.L__wine_spec_import_data_ptrs+1672(%rip)
	.size TranslateMDISysAccel, .-TranslateMDISysAccel

	.align 4
	.type TranslateMessage,@function
	.globl TranslateMessage
	.hidden TranslateMessage
TranslateMessage:
	jmpq *.L__wine_spec_import_data_ptrs+1680(%rip)
	.size TranslateMessage, .-TranslateMessage

	.align 4
	.type UpdateWindow,@function
	.globl UpdateWindow
	.hidden UpdateWindow
UpdateWindow:
	jmpq *.L__wine_spec_import_data_ptrs+1688(%rip)
	.size UpdateWindow, .-UpdateWindow

	.align 4
	.type RedrawWindow,@function
	.globl RedrawWindow
	.hidden RedrawWindow
RedrawWindow:
	jmpq *.L__wine_spec_import_data_ptrs+1696(%rip)
	.size RedrawWindow, .-RedrawWindow

	.align 4
	.type IsWindowUnicode,@function
	.globl IsWindowUnicode
	.hidden IsWindowUnicode
IsWindowUnicode:
	jmpq *.L__wine_spec_import_data_ptrs+1704(%rip)
	.size IsWindowUnicode, .-IsWindowUnicode

	.align 4
	.type CloseHandle,@function
	.globl CloseHandle
	.hidden CloseHandle
CloseHandle:
	jmpq *.L__wine_spec_import_data_ptrs+1720(%rip)
	.size CloseHandle, .-CloseHandle

	.align 4
	.type CreateFileA,@function
	.globl CreateFileA
	.hidden CreateFileA
CreateFileA:
	jmpq *.L__wine_spec_import_data_ptrs+1728(%rip)
	.size CreateFileA, .-CreateFileA

	.align 4
	.type ExitProcess,@function
	.globl ExitProcess
	.hidden ExitProcess
ExitProcess:
	jmpq *.L__wine_spec_import_data_ptrs+1736(%rip)
	.size ExitProcess, .-ExitProcess

	.align 4
	.type FreeLibrary,@function
	.globl FreeLibrary
	.hidden FreeLibrary
FreeLibrary:
	jmpq *.L__wine_spec_import_data_ptrs+1744(%rip)
	.size FreeLibrary, .-FreeLibrary

	.align 4
	.type GetCommandLineA,@function
	.globl GetCommandLineA
	.hidden GetCommandLineA
GetCommandLineA:
	jmpq *.L__wine_spec_import_data_ptrs+1752(%rip)
	.size GetCommandLineA, .-GetCommandLineA

	.align 4
	.type GetLocaleInfoA,@function
	.globl GetLocaleInfoA
	.hidden GetLocaleInfoA
GetLocaleInfoA:
	jmpq *.L__wine_spec_import_data_ptrs+1760(%rip)
	.size GetLocaleInfoA, .-GetLocaleInfoA

	.align 4
	.type GetModuleHandleA,@function
	.globl GetModuleHandleA
	.hidden GetModuleHandleA
GetModuleHandleA:
	jmpq *.L__wine_spec_import_data_ptrs+1768(%rip)
	.size GetModuleHandleA, .-GetModuleHandleA

	.align 4
	.type GetProcAddress,@function
	.globl GetProcAddress
	.hidden GetProcAddress
GetProcAddress:
	jmpq *.L__wine_spec_import_data_ptrs+1776(%rip)
	.size GetProcAddress, .-GetProcAddress

	.align 4
	.type GetStartupInfoA,@function
	.globl GetStartupInfoA
	.hidden GetStartupInfoA
GetStartupInfoA:
	jmpq *.L__wine_spec_import_data_ptrs+1784(%rip)
	.size GetStartupInfoA, .-GetStartupInfoA

	.align 4
	.type LoadLibraryA,@function
	.globl LoadLibraryA
	.hidden LoadLibraryA
LoadLibraryA:
	jmpq *.L__wine_spec_import_data_ptrs+1792(%rip)
	.size LoadLibraryA, .-LoadLibraryA

	.align 4
	.type MulDiv,@function
	.globl MulDiv
	.hidden MulDiv
MulDiv:
	jmpq *.L__wine_spec_import_data_ptrs+1800(%rip)
	.size MulDiv, .-MulDiv

	.align 4
	.type MultiByteToWideChar,@function
	.globl MultiByteToWideChar
	.hidden MultiByteToWideChar
MultiByteToWideChar:
	jmpq *.L__wine_spec_import_data_ptrs+1808(%rip)
	.size MultiByteToWideChar, .-MultiByteToWideChar

	.align 4
	.type WideCharToMultiByte,@function
	.globl WideCharToMultiByte
	.hidden WideCharToMultiByte
WideCharToMultiByte:
	jmpq *.L__wine_spec_import_data_ptrs+1816(%rip)
	.size WideCharToMultiByte, .-WideCharToMultiByte

	.align 4
	.type WriteFile,@function
	.globl WriteFile
	.hidden WriteFile
WriteFile:
	jmpq *.L__wine_spec_import_data_ptrs+1824(%rip)
	.size WriteFile, .-WriteFile

	.align 4
	.type CreateSemaphoreA,@function
	.globl CreateSemaphoreA
	.hidden CreateSemaphoreA
CreateSemaphoreA:
	jmpq *.L__wine_spec_import_data_ptrs+1832(%rip)
	.size CreateSemaphoreA, .-CreateSemaphoreA

	.align 4
	.type ReleaseSemaphore,@function
	.globl ReleaseSemaphore
	.hidden ReleaseSemaphore
ReleaseSemaphore:
	jmpq *.L__wine_spec_import_data_ptrs+1840(%rip)
	.size ReleaseSemaphore, .-ReleaseSemaphore

	.align 4
	.type WaitForSingleObject,@function
	.globl WaitForSingleObject
	.hidden WaitForSingleObject
WaitForSingleObject:
	jmpq *.L__wine_spec_import_data_ptrs+1848(%rip)
	.size WaitForSingleObject, .-WaitForSingleObject

	.align 4
	.type GlobalAlloc,@function
	.globl GlobalAlloc
	.hidden GlobalAlloc
GlobalAlloc:
	jmpq *.L__wine_spec_import_data_ptrs+1856(%rip)
	.size GlobalAlloc, .-GlobalAlloc

	.align 4
	.type GlobalFree,@function
	.globl GlobalFree
	.hidden GlobalFree
GlobalFree:
	jmpq *.L__wine_spec_import_data_ptrs+1864(%rip)
	.size GlobalFree, .-GlobalFree

	.align 4
	.type GetTickCount,@function
	.globl GetTickCount
	.hidden GetTickCount
GetTickCount:
	jmpq *.L__wine_spec_import_data_ptrs+1872(%rip)
	.size GetTickCount, .-GetTickCount

	.align 4
	.type Sleep,@function
	.globl Sleep
	.hidden Sleep
Sleep:
	jmpq *.L__wine_spec_import_data_ptrs+1880(%rip)
	.size Sleep, .-Sleep

	.align 4
	.type DisableThreadLibraryCalls,@function
	.globl DisableThreadLibraryCalls
	.hidden DisableThreadLibraryCalls
DisableThreadLibraryCalls:
	jmpq *.L__wine_spec_import_data_ptrs+1888(%rip)
	.size DisableThreadLibraryCalls, .-DisableThreadLibraryCalls

	.align 4
	.type lstrcmpW,@function
	.globl lstrcmpW
	.hidden lstrcmpW
lstrcmpW:
	jmpq *.L__wine_spec_import_data_ptrs+1896(%rip)
	.size lstrcmpW, .-lstrcmpW

	.align 4
	.type CoTaskMemFree,@function
	.globl CoTaskMemFree
	.hidden CoTaskMemFree
CoTaskMemFree:
	jmpq *.L__wine_spec_import_data_ptrs+1912(%rip)
	.size CoTaskMemFree, .-CoTaskMemFree

	.align 4
	.type CoInitialize,@function
	.globl CoInitialize
	.hidden CoInitialize
CoInitialize:
	jmpq *.L__wine_spec_import_data_ptrs+1920(%rip)
	.size CoInitialize, .-CoInitialize

	.align 4
	.type CoUninitialize,@function
	.globl CoUninitialize
	.hidden CoUninitialize
CoUninitialize:
	jmpq *.L__wine_spec_import_data_ptrs+1928(%rip)
	.size CoUninitialize, .-CoUninitialize



	.size __wine_spec_import_thunks, .-__wine_spec_import_thunks
	.section .note.GNU-stack,"",@progbits

