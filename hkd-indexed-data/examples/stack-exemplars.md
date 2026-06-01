# Transferable ecosystem patterns

These are public pattern families worth recognizing. Do not cargo-cult library machinery; extract the shape.

## HKD records

Libraries such as `barbies` and `higgledy` popularized records parameterized by a carrier.

Steal the idea when:

- every field has the same carrier variation;
- validation, decoding, forms, diagnostics, or projections repeat the same schema;
- `traverse` across fields is the desired operation.

Reject it when each field has unrelated behavior and the carrier buys only ornament.

## Heterogeneous products

Libraries such as `vinyl`, `sop-core`, and `generics-sop` show typed products over closed universes.

Steal the idea when:

- the index universe is closed;
- each index may have a different payload type;
- operations need witness-aware map, zip, traverse, fold, or lookup.

Reject it for open plugin maps. That is a registry, not a total product.

## Typed columns and query projections

Relational or dataframe-style encodings often use a column descriptor plus a payload family.

Steal the idea when:

- a column witness determines the cell type;
- encoded, decoded, vectorized, and diagnostic views share one schema;
- dense execution views are derived from a symbolic field universe.

Reject it when wire strings or positional arrays become the semantic owner.
