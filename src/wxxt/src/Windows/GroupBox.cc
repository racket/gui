/*								-*- C++ -*-
 * $Id: GroupBox.cc,v 1.1 1996/01/10 14:57:11 markus Exp $
 *
 * Purpose: group box choice panel item
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifdef __GNUG__
#pragma implementation "GroupBox.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxGroupBox
#include "wx.h"
#define  Uses_EnforcerWidget
#include "widgets.h"

//-----------------------------------------------------------------------------
// create and destroy group box
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxGroupBox, wxItem)

wxGroupBox::wxGroupBox(void) : wxItem()
{
    __type = wxTYPE_GROUP_BOX;
}

wxGroupBox::wxGroupBox(wxPanel *panel, char *label, int x, int y, int width,
		       int height, long style, char *name)
{
    __type = wxTYPE_GROUP_BOX;

    Create(panel, label, x, y, width, height, style, name);
}

Bool wxGroupBox::Create(wxPanel *panel, char *label, int x, int y, int width,
			int height, long style, char *name)
{
    ChainToPanel(panel, style, name);

    Bool vert   = (panel->GetLabelPosition() == wxVERTICAL);

    label = wxGetCtlLabel(label);

    // create frame
    X->frame = X->handle = XtVaCreateManagedWidget
	(name, xfwfEnforcerWidgetClass, parent->GetHandle()->handle,
	 XtNlabel,       label,
	 XtNalignment,   vert ? XfwfTop : XfwfTopLeft,
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  label_fg->GetPixel(cmap),
	 XtNfont,        label_font->GetInternalFont(),
	 XtNframeType,   XfwfChiseled,
	 XtNframeWidth,  2,
	 XtNhighlightThickness, 0, XtNtraversalOn, FALSE,
	 NULL);
    // panel positioning
    panel->PositionItem(this, x, y, 
			(width  > -1 ? width  : wxGROUP_BOX_WIDTH),
			(height > -1 ? height : wxGROUP_BOX_HEIGHT));
    return TRUE;
}
