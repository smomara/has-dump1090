# has-dump1090
A real-time aircaft tracker written in Haskell that decodes ADS-B messages from aircraft transponders using an RTL-SDR device.

# What it does
Listens to radio signals from nearby aircraft on 1090 MHz and display their:
* Location (latitude + longitude)
* Altitude
* Speed
* Direction (track)
* Vertical rate (climb/descent)
* Flight number/callsign
* Aircraft registration (hex code)

Example output:
```
-----------------------------------------------------------------------------------------------
HEX      | CALLSIGN | POSITION             | ALTITUDE   | SPEED      | TRACK      | VERT RATE      
-----------------------------------------------------------------------------------------------
a02e0a   | AAL2051  | unknown              | 38050 ft   | 433 kt     | 225.0°     | 64 ft/min      
a78945   | unknown  | 39.0297, -77.1098    | 9875 ft    | 281 kt     | 50.4°      | 2432 ft/min    
-----------------------------------------------------------------------------------------------
```

# Requirements
* GHC
* Cabal
* RTL-SDR USB device
* librtlsdr development files

# Quick Start
```bash
# Install dependencies (Ubuntu/Debian)
sudo apt install librtlsdr-dev

# Build and run
git clone https://github.com/smomara/has-dump1090.git
cd has-dump1090
cabal build
cabal run
```

# License
MIT License

# Contributions
Contributions welcome! Feel free to submit a PR
