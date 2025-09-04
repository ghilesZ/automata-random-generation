# Diverse Regular Language Generator

This tool generates random regular expressions, converts them into
finite automata, and computes basic diversity statistics such as
**richness**, **variety**, and the **Simpson index**.  It supports
both **uniform** and **balanced** random tree generation methods, and
can optionally keep intermediate representations (trees, regexps,
automata) as images.


## Usage

```bash
dune exec ./bin/main.exe -- [options]
````

### Options

* `-time`
  Show timing information for each step (tree → regexp → automaton → determinization → minimization).

* `-verbose`
  Print detailed statistics about generated automata (e.g., number of states, transitions, imbalance).

* `-svg`
  Output the minimized automaton as an SVG file.

* `-keep`
  Keep all intermediate outputs (`.png` / `.svg`) of the generation process:

  * The initial random tree
  * The derived regular expression
  * The nondeterministic automaton
  * The determinized automaton

* `-uniform`
  Use **uniform random tree generation**.

* `-balanced`
  Use **balanced random tree generation** (default).

* `-histogram <n>`
  Generate `n` random automata and build a histogram of their frequencies (after minimization & normalization).
  Prints:

  * The most common language (as a regexp)
  * Its relative frequency
  * Richness (# unique automata)
  * Variety (richness / total)
  * Simpson index (diversity measure)

* `-size <n>`
  Set the size of the generated random trees (default: `10`).

---

## Output Example

For example, running:

```bash
./program -histogram 1000 -size 12 -time -verbose
```

might print:

```
Generation using balanced method
------------------------------
Most common language : (a|b)*
Appearing 12.50% of the time
richness: 200
variety: 0.200000
simpson index: 0.123456
```

---

## Measures

* **Richness**: number of unique automata (languages) observed.
* **Variety**: `richness / total`, the proportion of uniqueness.
* **Simpson Index**: probability that two randomly chosen automata are of the same type (lower = more diverse).

---

## Workflows

* Generate and visualize a single random automaton:

  ```bash
  ./program -svg -size 15
  ```

* Benchmark the distribution of languages with uniform sampling:

  ```bash
  ./program -uniform -histogram 500
  ```

* Collect statistics with verbose output:

  ```bash
  ./program -verbose -histogram 1000
  ```

---

