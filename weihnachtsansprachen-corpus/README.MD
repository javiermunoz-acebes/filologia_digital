# Weihnachtsansprachen-Korpus

A plain-text corpus of German federal Christmas addresses (*Weihnachtsansprachen*), compiled for digital philological and computational text analysis research. The corpus is designed for incremental expansion across speakers and periods.

## Current coverage

| Speaker | Role | Years | Texts |
|---|---|---|---|
| Konrad Adenauer | Bundeskanzler | 1949–1962 | 14 |

## Repository structure

```
weihnachtsansprachen-corpus/
├── README.md
├── LICENSE
├── CORPUS_METADATA.csv          # Index of all texts
├── speeches/
│   └── adenauer/                # One subfolder per speaker (surname, lowercase)
│       ├── 1949_adenauer_weihnachtsansprache.txt
│       └── ...
└── scripts/
    └── corpus_manager.py        # Filename normalization and metadata generation
```

**Filename convention:** `{year}_{speaker_surname}_weihnachtsansprache.txt`

All files are encoded in **UTF-8**. Original orthography is preserved where sources permit; editorial interventions are documented in `CORPUS_METADATA.csv`.

## Metadata

`CORPUS_METADATA.csv` contains the following fields:

| Field | Description |
|---|---|
| `id` | Unique text identifier (e.g. `adenauer_1949`) |
| `speaker` | Speaker surname (lowercase) |
| `year` | Year of delivery |
| `date` | Full date (YYYY-MM-DD) if known |
| `role` | Institutional role at time of delivery |
| `filename` | Normalized filename |
| `word_count` | Token count (whitespace-tokenized) |
| `source` | Primary source or transcription origin |
| `notes` | Editorial notes |
| `transcription_status` | `verified`, `unverified`, or `ocr` |

## Scripts

### `corpus_manager.py`

Normalizes filenames, corrects known typos, and generates or updates `CORPUS_METADATA.csv`. Requires Python 3.10+ and no external dependencies.

```bash
# Dry run (no changes to disk)
python scripts/corpus_manager.py --dir speeches/adenauer --dry-run

# Process a single speaker folder
python scripts/corpus_manager.py --dir speeches/adenauer

# Process all speaker folders at once
python scripts/corpus_manager.py --dir speeches --all-speakers
```


## Intended use

This corpus is designed for:

- Stylometric and authorship analysis
- Diachronic lexical and rhetorical studies
- Pedagogical use in digital humanities seminars

## License

Corpus structure, metadata, and scripts: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).  
The texts themselves are reproduced for scholarly, non-commercial purposes only. Original sources are documented in `CORPUS_METADATA.csv`.

## How to cite

> Muñoz-Acebes, Francisco Javier (2026). *Weihnachtsansprachen-Korpus*. GitHub.  
> https://github.com/[your-username]/weihnachtsansprachen-corpus  
> *(Zenodo DOI pending)*

## Contributing

Contributions welcome — new speakers, corrected transcriptions, or additional metadata. Please open an issue or pull request and follow the existing folder and filename conventions.
