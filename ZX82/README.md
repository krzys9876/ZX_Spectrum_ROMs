# ZX82 ROM Modification
- SIO support instead of tape
- load from USB-UART bridge
  - NO hardware flow control (still to implement)
  - used with RealTerm and a few miliseconds of a delay between characters
- all multi-part programs MUST be split to parts using splittap.py	 
- printer channel forwarder do SIO to get program listing in printable format via USB-UART
