# Enigma Machine Haskell (Second Year Project)
 An Enigma machine (used by Germans in WWII) that has rotors with knock-on positions. The first rotor will keep advancing by 1 step & the offset advances with every key press as well. The second and third rotor offsets advances with every previous rotor knock-on position achieved. There is also the reflector which reflects the reflector board pair of the letter encoded first starting from the right rotor all the way to the left rotor. Then that reflected out letter pair will proceed to be encoded from the left rotor to the right rotor which then gets output. Depending on whether the Enigma machine is doing the SimpleEnigma or SteckeredEnigma, will it then use the steckerboard to swap each of the letters of the message to be encoded before it is passed through the rotors and swap the letters of the encode message before/when it outputs the final encoded message. Encoding & decoding goes both ways.
