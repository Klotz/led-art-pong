%    File:	protocol.hrl
%    Author:	Huib Verweij
%    Created:	Tue Oct 22 16:31:42 1996
%    Purpose:   Define constants for use in the protocol with the 8051 hardware controller
 
-ifndef(PROTOCOL_HRL).
-define(PROTOCOL_HRL, true).

-define(HEADER,255).
-define(ID,0).
-define(CMD_SETSTRIPE,16).
-define(CMD_SETROW,12).
-define(CMD_DEMO,254).
-define(CMD_FADEALLTOWHITE,22).
-define(CMD_FADEALLTOBLACK,21).

-define(TX_BUTTONSTATE,40).

-endif.

