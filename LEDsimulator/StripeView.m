//
//  StripeView.m
//  LEDsimulator
//
//  Created by Huib Verweij on 03-10-09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import "StripeView.h"


@implementation StripeView

@synthesize color;

- (id)initWithFrame:(NSRect)frameRect color:(NSColor *)initialColor {
	[super initWithFrame:frameRect];
	[self setColor:initialColor];
	return self;
}

- (BOOL)isOpaque {
	return NO;
}

- (void)drawRect:(NSRect)rect
{
    // Get the graphics context that we are currently executing under
    NSGraphicsContext* gc = [NSGraphicsContext currentContext];
	
    // Save the current graphics context settings
    [gc saveGraphicsState];
	
    // Set the color in the current graphics context for future draw operations
    [[NSColor grayColor] setStroke];
    [color setFill];
	
    // Create our circle path
    NSRect drawRect = NSMakeRect(1, 1, 14, 14);
    NSBezierPath* circlePath = [NSBezierPath bezierPath];
    [circlePath appendBezierPathWithOvalInRect: drawRect];
	
    // Outline and fill the path
    [circlePath stroke];
    [circlePath fill];
	
    // Restore the context to what it was before we messed with it
    [gc restoreGraphicsState];
}

@end
