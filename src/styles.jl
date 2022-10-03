
"""
    MetadataStyle(M::Type)

Given the metadata type (`M`), returns a subtype of `MetadataStyle` that regulates how
instances of `M` are propagated between operations.
"""
abstract type MetadataStyle end
MetadataStyle(x::MetadataStyle) = x
MetadataStyle(x) = MetadataStyle(typeof(x))
MetadataStyle(T::Type) = MetadataDefault()
function MetadataStyle(x::MetadataStyle, y::MetadataStyle)
    _catch_unkown(combine_styles(x, y), combine_styles(y, x), x, y)
end
MetadataStyle(x, y) = MetadataStyle(MetadataStyle(x), MetadataStyle(y))
@inline MetadataStyle(x, y, zs...) = MetadataStyle(MetadataStyle(x, y), zs...)

"""
    MetadataPersistent

Subtype of `MetadataStyle` indicated that its associated metadata should persist
across operations that copy its contextual data.

See also: [`MetadataStyle`](@ref)
"""
struct MetadataPersistent <: MetadataStyle end

"""
    MetadataDefault

The default type returned by `MetadataStyle(type)`.

See also: [`MetadataStyle`](@ref)
"""
struct MetadataDefault <: MetadataStyle end

"""
    MetadataNamed

Subtype of `MetadataStyle` indicating that it's associated data is a `NamedTuple` where
each field is also metadata.

See also: [`MetadataStyle`](@ref)
"""
struct MetadataNamed <: MetadataStyle end

struct MetadataUnknown <: MetadataStyle end

"""
    combine_styles(style::MetadataStyle...) -> MetadataStyle

Decides which `MetadataStyle` to use when combining metadata from multiple objects.
"""
combine_styles(::MetadataDefault, s) = s
combine_styles(::MetadataDefault, ::MetadataDefault) = MetadataDefault()
combine_styles(s::S, ::S) where {S} = s
combine_styles(s1, s2) = MetadataUnknown()

## _catch_unkown
_catch_unkown(s, ::MetadataUnknown, @nospecialize(x), @nospecialize(y)) = s
_catch_unkown(::MetadataUnknown, s, @nospecialize(x), @nospecialize(y)) = s
_catch_unkown(s::S, ::S, @nospecialize(x), @nospecialize(y)) where {S} = s
# TODO error for no convergence
@noinline function _catch_unkown(::MetadataUnknown, ::MetadataUnknown, @nospecialize(x), @nospecialize(y))
    error("Failed to combine `MetadataStyle`s  $(x) and $(y))")
end
# TODO error for converging on disparate types
@noinline function _catch_unkown(@nospecialize(s1), @nospecialize(s2), @nospecialize(x), @nospecialize(y))
    error("""
conflicting MetadataStyle rules defined
  MetadataStyle(::$(nameof(x)), ::$(nameof(y))) = $(nameof(s1))()
  MetadataStyle(::$(nameof(y)), ::$(nameof(x))) = $(nameof(s2))()
One of these should be undefined (and thus return MetadataUnknown).""")
end

