# Functional Portfolio Optimizer

A functional parallel portfolio optimizer implemented in Haskell.  
It uses historical stock data to simulate portfolios, calculate key performance metrics (like Sharpe ratio), and identify the optimal portfolio among a set of combinations.

---

## Installation

To build and run the project, make sure you have [GHC](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/) installed.

### Clone the repository:
```bash
git clone https://github.com/yourusername/Functional-Portfolio-Optimizer.git
cd Functional-Portfolio-Optimizer
```

### Build the project:
```bash
cabal build
```

---

## Running the Optimizer

Make sure you have your stock CSV files inside the `stocks/` folder (one per ticker).  
Each file must be named like `AAPL.csv`, `GOOG.csv`, etc., and follow the standard Yahoo Finance format.

Then, execute:

```bash
cabal run Functional-Portifolio-Optimizer
```

The optimizer will:
1. Parse historical data for different stock combinations.
2. Generate random portfolios with different weight distributions.
3. Calculate expected return, volatility, and Sharpe ratio.
4. Output the best portfolio to `best_portfolio.csv`.

---

##  Requirements & Dependencies

- `base`
- `vector`
- `bytestring`
- `cassava` (CSV parsing)
- `random` & `random-shuffle` (portfolio generation)
- `parallel` & `deepseq` (parallel simulation)
- `containers` (caching data)
- `split`, `async`, `time`, `progress`

These are managed automatically by Cabal.

---

##  Expected Results

When run with:
```haskell
generatePortfolios 27 20 seed
```

### Sample Output:
```
Total execution time: 181.82 seconds
Best Sharpe ratio found: 3.484
```

The final portfolio is saved in `best_portfolio.csv` and contains:
- The list of tickers used.
- The portfolio weights.
- Annualized return.
- Annualized volatility.
- Sharpe ratio.

---

##  Performance Notes

- The application uses **parallel map (parMap)** to fully utilize multicore systems.
- Bottlenecks like covariance matrix and return matrix computations are optimized using `deepseq` and caching.
- Profiling tools such as `ghc-prof` can be used to further analyze performance.

---

## Disclaimer
Generative AI tools were used to generate and review code