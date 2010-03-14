//
//  LEDsimulator.m
//  LEDsimulator
//
//  Created by Huib Verweij on 27-09-09.
//  Copyright 2009 __MyCompanyName__. All rights reserved.
//

#import "LEDsimulator.h"


@implementation LEDsimulator

@synthesize status;
@synthesize serverInfo;
@synthesize serverSocket;

- (void) awakeFromNib {
	NSLog(@"LEDsimulator awakeFromNib starting\n");
	[box setFillColor:[NSColor blackColor]];
	[self updateSocketStatus:nil];
	for (int i = 0; i < NUMBER_OF_LEDS ; i++) {
		NSRect frame = NSMakeRect(20+i*26.0, 10.0, 20.0, 20.0);
		StripeView *stripeView = [[StripeView alloc] initWithFrame:frame color:[NSColor colorWithCalibratedRed:LEDBACKGROUNDCOLOR green:LEDBACKGROUNDCOLOR blue:LEDBACKGROUNDCOLOR alpha:1.0]];
		[[box contentView] addSubview:stripeView];
		[stripeView release];
	}

	server = [[NSThread alloc] initWithTarget:self selector:@selector(omni_server) object:nil];
	[server setName:@"Network server"];
	[server start];
	NSLog(@"LEDsimulator awakeFromNib done\n");
}

- (void) omni_server {
	NSLog(@"LEDsimulator (omni)server starting\n");
	unsigned short serverPort = PORT_NUMBER; 
	NSAutoreleasePool *mainPool; 
	mainPool = [[NSAutoreleasePool alloc] init]; 
	serverSocket = [ONTCPSocket tcpSocket];
	[serverSocket startListeningOnLocalPort: serverPort allowingAddressReuse:YES];
	[self updateSocketStatus:nil];
	do { 
		ONTCPSocket *connectionSocket;
		NSAutoreleasePool *loopPool; 
		loopPool = [[NSAutoreleasePool alloc] init]; 
		connectionSocket = [serverSocket acceptConnectionOnNewSocket];
		[self process_connection:connectionSocket];
		[loopPool release]; 
	} while (1); 
	[mainPool release];
	NSLog(@"LEDsimulator (omni)server done\n");
}

- (void) process_connection:(ONTCPSocket *)connectionSocket { 
	stream = [ONSocketStream streamWithSocket:connectionSocket];
	// Update display with name of client. 
	ONHost *remote = [connectionSocket remoteAddressHost];
	NSLog(@"Connected to client %@\n", [remote hostname]);
	[self updateSocketStatus:connectionSocket];
	@try {
		while ([connectionSocket isConnected]) {
			NSAutoreleasePool *loopPool; 
			loopPool = [[NSAutoreleasePool alloc] init]; 
			NSMutableData *clientData = [[NSMutableData alloc] initWithLength:0];
			[clientData appendData:[stream readDataOfLength:7]];
			[self process_commands:clientData];
			[clientData release];
			[loopPool release]; 
		}
	}
	@catch (NSException *exception) {
		NSLog(@"Exception %@:%@", [exception name], [exception reason]); 
		[connectionSocket abortSocket];
	}
	@finally {
		connectionSocket = nil;
		[self updateSocketStatus:connectionSocket];
		NSLog(@"Disconnected from client %@\n", [remote hostname]);
	}
}

- (void)updateSocketStatus:(ONTCPSocket *)socket {
	if (socket == nil) {
		if (serverSocket != nil) {
			[status setStringValue:[NSString stringWithFormat:@"listening on %@.", [serverSocket localAddress]]];
		}
		else {
			[status setStringValue:@"initialising"];
		}

	}
	else {
		[status setStringValue: [NSString stringWithFormat:@"connected to %@", [[socket remoteAddressHost] hostname]]];
	}		
}

- (void)process_commands:(NSData *)command {
	const unsigned char *bytes = [command bytes];
	unsigned char byte;
	NSColor *color;
	byte = bytes[0];
	if (bytes[0] == (unsigned char)0xff) {
		if (bytes[1] == CMD_SETSTRIPE) {
			NSUInteger stripe = bytes[2];
			if (stripe > 0 && stripe <= NUMBER_OF_LEDS) {
				CGFloat red = bytes[3]/255.0;
				CGFloat green = bytes[4]/255.0;
				CGFloat blue = bytes[5]/255.0;
				if (red == 0 && green == 0 && blue == 0) { // Off pixels look better sort-of transparent
					color = [NSColor colorWithCalibratedRed:LEDBACKGROUNDCOLOR green:LEDBACKGROUNDCOLOR blue:LEDBACKGROUNDCOLOR alpha:1.0];
				}
				else {
					color = [NSColor colorWithCalibratedRed:red green:green blue:blue alpha:1.0];
				}
				[self setStripe:stripe withColor:color];
			}
		}
		else if (bytes[1] == (char)CMD_SETROW) {
			NSColor *color = [NSColor colorWithCalibratedRed:bytes[2]/255.0 green:bytes[3]/255.0 blue:bytes[4]/255.0 alpha:1.0];
			for (int stripe = 1; stripe <= NUMBER_OF_LEDS ; stripe++) {
				[self setStripe:stripe withColor:color];
			}
		}
	}
}

- (void)setStripe:(NSUInteger)stripe withColor:(NSColor *)color {
	NSArray *stripes = [[box contentView] subviews];
	StripeView *stripeView = [stripes objectAtIndex:stripe - 1]; // External adressing ranges from 1 - 32.
	[stripeView setColor:color];
	[stripeView setNeedsDisplay:YES];
}


- (unsigned char)checksum:(unsigned char [])bytes nr_of_bytes:(int)count {
	unsigned char checksum = 0;
	while (count > 0) {
		checksum += bytes[--count];
		NSLog(@"bytes[--count]=%d, checksum=%d.", bytes[count], checksum);
	}
	return checksum;
}


- (IBAction)buttonOnePressed:(id)sender {
	[self buttonPressed:1];
}
- (IBAction)buttonTwoPressed:(id)sender {
	[self buttonPressed:2];
}
- (void)buttonPressed:(int)button {
	NSLog(@"Button %d pressed.", button);
	unsigned char bytes[5] = {0xff, 0x28, (unsigned char)button, 150, 0x00};
	bytes[4] = [self checksum:bytes nr_of_bytes:4];
	NSMutableData *clientData = [[NSMutableData alloc] initWithBytes:bytes length:5];
	[stream writeData:clientData];
	[clientData release];
}


- (void)dealloc
{
    [[NSNotificationCenter defaultCenter] removeObserver:self];
    [super dealloc];
}

@end
