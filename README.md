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
* ADS-B Extended Squitter (DF17) message decoding:
	* Aircraft Identification (Callsign)
	* Aircraft Categories

## Planned Features
* Additional ADS-B message type decoding
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
<hex payload> [<DF type> ICAO:<address>] <decoded information>
```

Example:
```
Processing test messages:
Test message: 8d4840d6202cc371c32ce0576098 [DFExtendedSquitter ICAO:4840d6] {DF=DFExtendedSquitter ICAO=4840d6 Type=AircraftID Flight=KLM1023 Category=NoCategory}

Processing binary file:
8d45ac2d9904d910613f94ba81b5 [DFExtendedSquitter ICAO:45ac2d] {DF=DFExtendedSquitter ICAO=45ac2d}
5d45ac2da5e9cb [DFAllCallReply ICAO:45ac2d] {DF=DFAllCallReply ICAO=45ac2d}
8d45ac2d583561285c4fa686fcdc [DFExtendedSquitter ICAO:45ac2d] {DF=DFExtendedSquitter ICAO=45ac2d Alt=9550ft}
a00006979b580030400000df4221 [DFCommAAltRequest ICAO:45ac2d] {DF=DFCommAAltRequest ICAO=45ac2d Alt=9575ft Squawk=6427}
5d45ac2da5e9cb [DFAllCallReply ICAO:45ac2d] {DF=DFAllCallReply ICAO=45ac2d}
5d45ac2da5e9cb [DFAllCallReply ICAO:45ac2d] {DF=DFAllCallReply ICAO=45ac2d}
02a186b39408d0 [DFShortAirSurveillance ICAO:45ac2d] {DF=DFShortAirSurveillance ICAO=45ac2d Alt=9875ft}
200006b31828c8 [DFSurveillanceAlt ICAO:45ac2d] {DF=DFSurveillanceAlt ICAO=45ac2d Alt=9875ft Squawk=6525}
```
