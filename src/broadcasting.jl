
"""
    MetadataArrays.MetadataArrayStyle{S}

Subtype of `BroadcastStyle` for `MetadataArray`, where `S` is the `BroadcastStyle`
of the parent array. This helps extract, combine, and propagate metadata from arrays.

"""
struct MetadataArrayStyle{S<:BroadcastStyle} <: Broadcast.AbstractArrayStyle{Any} end
MetadataArrayStyle(::S) where {S} = MetadataArrayStyle{S}()
MetadataArrayStyle(::S, ::Val{N}) where {S,N} = MetadataArrayStyle(S(Val(N)))
MetadataArrayStyle(::Val{N}) where {N} = MetadataArrayStyle{Broadcast.DefaultArrayStyle{N}}()
function MetadataArrayStyle(a::BroadcastStyle, b::BroadcastStyle)
    style = BroadcastStyle(a, b)
    if style === Broadcast.Unknown()
        return Broadcast.Unknown()
    else
        return MetadataArrayStyle(style)
    end
end
function Base.BroadcastStyle(T::Type{<:MetadataArray})
    MetadataArrayStyle{typeof(BroadcastStyle(fieldtype(T, :parent)))}()
end
function Base.BroadcastStyle(::MetadataArrayStyle{A}, ::MetadataArrayStyle{B}) where {A,B}
    style = BroadcastStyle(A(), B())
    isa(style, Broadcast.Unknown) ? Broadcast.Unknown() : MetadataArrayStyle(style)
end

# Resolve ambiguities
# for all these cases, we define that we win to be the outer style regardless of order
for B in (:BroadcastStyle, :(Broadcast.DefaultArrayStyle), :(Broadcast.AbstractArrayStyle), :(Broadcast.Style{Tuple}),)
    @eval begin
        #Base.BroadcastStyle(::MetadataArrayStyle{A}, b::$B) where {A} = MetadataArrayStyle(A(), b)
        Base.BroadcastStyle(b::$B, ::MetadataArrayStyle{A}) where {A} = MetadataArrayStyle(b, A())
    end
end

# TODO need combine_metadata to extract metadata info correctly
# We need to implement copy because if the wrapper array type does not support setindex
# then the `similar` based default method will not work
function Broadcast.copy(bc::Broadcast.Broadcasted{MetadataArrayStyle{S}}) where {S}
    copy(Broadcast.Broadcasted{S}(bc.f, map(dropmeta, bc.args), axes(bc)))
end
