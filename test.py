def pure_tone(seconds, frequency):
  from os import system
  system(f'timeout -s SIGKILL {seconds} speaker-test --frequency {frequency} --test sine > /dev/null 2> /dev/null')

# Equal temperament with A4 = 440
def hz(name, octave):
  int_of = {
    'C': 0,
    'C#': 1,
    'Db': 1,
    'D': 2,
    'D#': 3,
    'Eb': 3,
    'E': 4,
    'F': 5,
    'F#': 6,
    'Gb': 6,
    'G': 7,
    'G#': 8,
    'Ab': 8,
    'A': 9,
    'A#': 10,
    'Bb': 10,
    'B': 11,
  }
  offset = (int_of[name] + 12*octave) - (int_of['A'] + 12*4)
  return 2**(offset/12) * 440

from threading import Thread

# A fragment of music is a function which takes in a total duration and returns a thread to play it
# type Fragment = int → Thread

# Fragment
def note(name, octave):
  return lambda seconds: Thread(target = lambda: pure_tone(seconds, hz(name, octave)))

# Fragment
def rest():
  from time import sleep
  return lambda seconds: Thread(target = lambda: sleep(seconds))

# Fragment ‥ → Fragment
def par(*fragments):
  def do_par(seconds):
    for fragment in fragments:
      fragment(seconds).start()
  return lambda seconds: Thread(target = lambda: do_par(seconds))

# ((int × Fragment) ∪ Fragment) ‥ → Fragment
# Fragments may be annotated by relative weights.
# By default, weight = 1.
def seq(*fragments):
  def do_seq(seconds):
    weighted_fragments = [
      fragment if type(fragment) is tuple else (1, fragment)
      for fragment in fragments
    ]
    total_weight = sum(weight for weight, _ in weighted_fragments)
    for weight, fragment in weighted_fragments:
      fragment = fragment(seconds * weight/total_weight)
      fragment.start()
      fragment.join()
  return lambda seconds: Thread(target = lambda: do_seq(seconds))

# int Fragment → Fragment
def repeat(reps, fragment):
  def do_repeat(seconds):
    for _ in range(reps):
      thread = fragment(seconds/reps)
      thread.start()
      thread.join()
  return lambda seconds: Thread(target = lambda: do_repeat(seconds))

# https://www.youtube.com/watch?v=vDKxtb510NQ
par(
  # Soprano
  seq(
    seq(note('A', 5), seq(note('B', 5), note('C#', 6))),
    seq(note('D#', 6), seq(rest(), note('D#', 6))),
    seq(note('E', 6), note('B', 5), note('B', 5), note('D', 6)),
    seq((3, note('C#', 6)), note('B', 5)),
    seq(seq(note('C#', 6), note('D#', 6)), note('E', 6), note('E', 6), note('D#', 6)),
    seq(note('E', 6), note('A', 5)),
    seq(seq(note('B', 5), note('C#', 6)), note('D#', 6)),
    seq(rest(), note('D#', 6), note('E', 6), note('B', 5)),
    seq(seq(note('B', 5), note('D', 6)), note('C#', 6)),
  ),
  # Alto
  seq(
    seq(note('E', 5), seq(note('E', 5), seq(note('D', 5), note('C#', 5)))),
    seq(note('G#', 5), seq(rest(), note('G#', 5))),
    seq(
      seq((3, note('G#', 5)), note('A', 5)),
      seq(note('G#', 5), note('F#', 5), note('G#', 5), note('E', 5))),
    seq((3, note('A', 5)), note('G#', 5)),
    seq(
      seq(note('A', 5), note('B', 5)), note('C#', 6), note('B', 5),
      seq(seq(note('A', 5), note('G#', 5)), note('A', 5))),
    seq(note('G#', 5), note('E', 5)),
    seq(seq(note('E', 5), note('E', 5)), note('F#', 5)),
    seq(
      rest(), seq(note('F#', 5), seq(note('G#', 5), note('A', 5))),
      seq(note('G#', 5), note('F#', 5)), seq(note('G#', 5), note('A', 5))
    ),
    seq(seq(note('B', 5), note('G#', 5)), note('A', 5)),
  ),
  # Tenor
  seq(
    seq(note('C#', 5), seq(note('B', 4), note('F#', 5))),
    seq(note('F#', 5), seq(rest(), note('C', 5))),
    seq(seq(note('C#', 5), note('D', 5)), note('E', 5), note('E', 5), note('E', 5)),
    seq((3, note('E', 5)), note('E', 5)),
    seq(seq(note('E', 5), note('F#', 5)), note('G', 5), note('F#', 5), note('B', 4)),
    seq(note('B', 4), note('C#', 5)),
    seq(seq(note('B', 4), note('A', 4)), note('A', 4)),
    seq(rest(), note('B', 4), note('B', 4), note('E', 5)),
    seq(seq(note('E', 5), note('E', 5)), note('E', 5)),
  ),
  # Bass
  seq(
    seq(note('A', 4), seq(note('G#', 4), note('A#', 4))),
    seq(note('C', 5), seq(rest(), note('G#', 4))),
    seq(
      note('C#', 5), seq(note('B', 4), note('A', 4)), note('G#', 4), note('F#', 4),
      note('E', 4), note('D', 4), note('C#', 4), note('B', 3)
    ),
    seq((3, note('A', 3)), note('E', 4)),
    seq(note('A', 4), note('G#', 4), note('B', 4), note('B', 3)),
    seq(note('E', 4), note('A', 4)),
    seq(seq(note('G#', 4), note('G', 4)), note('F#', 4)),
    seq(seq(rest(), note('B', 3)), seq((3, note('E', 4)), note('F#', 4))),
    seq(seq(note('G#', 4), seq(note('F#', 4), note('E', 4))), note('A', 4)),
  ),
)(45).start()
