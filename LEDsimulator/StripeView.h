//
//  StripeView.h
//  LEDsimulator
//
//  Created by Huib Verweij on 03-10-09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>


@interface StripeView : NSView {
	NSColor *color;
}

@property (retain) NSColor *color;

- (id)initWithFrame:(NSRect)frameRect color:(NSColor *)color;

@end
