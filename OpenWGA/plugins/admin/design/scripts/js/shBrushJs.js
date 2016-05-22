/*******************************************************************************
 *Copyright 2009, 2010 Innovation Gate GmbH. All Rights Reserved.
 *
 *This file is part of the OpenWGA server platform.
 *
 *OpenWGA is free software: you can redistribute it and/or modify
 *it under the terms of the GNU General Public License as published by
 *the Free Software Foundation, either version 3 of the License, or
 *(at your option) any later version.
 *
 *In addition, a special exception is granted by the copyright holders
 *of OpenWGA called "OpenWGA plugin exception". You should have received
 *a copy of this exception along with OpenWGA in file COPYING.
 *If not, see <http://www.openwga.com/gpl-plugin-exception>.
 *
 *OpenWGA is distributed in the hope that it will be useful,
 *but WITHOUT ANY WARRANTY; without even the implied warranty of
 *MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *GNU General Public License for more details.
 *
 *You should have received a copy of the GNU General Public License
 *along with OpenWGA in file COPYING.
 *If not, see <http://www.gnu.org/licenses/>.
 ******************************************************************************/
dp.sh.Brushes.JScript = function()
{
	var keywords =	'abstract boolean break byte case catch char class const continue debugger ' +
					'default delete do double else enum export extends false final finally float ' +
					'for function goto if implements import in instanceof int interface long native ' +
					'new null package private protected public return short static super switch ' +
					'synchronized this throw throws transient true try typeof var void volatile while with';

	this.regexList = [
		{ regex: dp.sh.RegexLib.SingleLineCComments,				css: 'comment' },			// one line comments
		{ regex: dp.sh.RegexLib.MultiLineCComments,					css: 'comment' },			// multiline comments
		{ regex: dp.sh.RegexLib.DoubleQuotedString,					css: 'string' },			// double quoted strings
		{ regex: dp.sh.RegexLib.SingleQuotedString,					css: 'string' },			// single quoted strings
		{ regex: new RegExp('^\\s*#.*', 'gm'),						css: 'preprocessor' },		// preprocessor tags like #region and #endregion
		{ regex: new RegExp(this.GetKeywords(keywords), 'gm'),		css: 'keyword' }			// keywords
		];

	this.CssClass = 'dp-c';
}

dp.sh.Brushes.JScript.prototype	= new dp.sh.Highlighter();
dp.sh.Brushes.JScript.Aliases	= ['js', 'jscript', 'javascript'];
