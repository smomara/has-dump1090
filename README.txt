# has-dump1090

A Haskell implementation of a Mode-S decoder for aviation transponder signals, inspired by dump1090

## Overview

has-dump1090 decodes Mode-S messages from aircraft transponders, similar to the popular dump1090 utility. It's currently under development, with a focus on building complete demodulation and decoding components before moving to real-time SDR integration.

## Current Features

### Demodulator
* Processes raw IQ samples
* Signal detection and filtering
* Preamble detection and synchronization
* Sample to bit conversion

### Decoder
* Support for multiple Mode-S message formats
* ICAO address cache system for improved message correlation
* Error correction capabilities
	* Single-bit error correction for DF11
	* Up to double-bit error correction for DF17
* CRC validation and correction

## Planned Features
* Full message content decoding
	* Aircraft position
	* Velocity
	* Aircraft identity
	* Altitude
	* Status information
* RTL-SDR device integration for real-time reception

## Prerequisites
* GHC (Glasgow Haskell Compiler)
* Cabal build system
* RTL-SDR library and headers (for future SDR support)

## Installation
```bash
# Clone the repository
git clone https://github.com/smomara/has-dump1090.git
cd has-dump1090

# Build the project
cabal build
```

## Current Usage
Currently supports processing a binary input file for testing and development:
```bash
cabal run
```

Output format:
```
<hex payload> [<DF type> ICAO:<address>]
```

Example:
```
8d45ac2d583561285c4fa686fcdc [DFExtendedSquitter ICAO:45ac2d]
```
