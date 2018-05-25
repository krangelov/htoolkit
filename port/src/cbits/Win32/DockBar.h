#ifndef DOCKBAR_H
#define DOCKBAR_H

void RelayoutFrameBars();

void DockToolBar(HWND hControlBar, HWND hWnd, int nBandNum, int nPosition, int nOffset);
void DockToolBarToRect(HWND hControlBar, HWND hWnd, RECT rect);
void UndockToolBar(HWND hControlBar, HWND hWnd);

void GetToolBarSize(HWND hToolBar, SIZE *pSize);

#endif
