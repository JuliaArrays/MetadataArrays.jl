
struct MetadataUnitRange{T, P, M} <: AbstractUnitRange{T}
    parent::P
    metadata::M
end

const ArrayMetadata{V, P, M, S} = Union{
    MetadataDict{String, V, P, M},
    MetadataDict{Symbol, V, P, M}
}

"""
    MetadataArray(x::AbstractArray, md::Union{AbstractDict{Symbol}, NamedTuple})
    MetadataArray(x::AbstractArray, md::Pair{Symbol}) -> MetadataArray(x, Dict(md...))
    MetadataArray(x::AbstractArray; md...) -> MetadataArray(x, NamedTuple(md))

Custom `AbstractArray` object to store an `AbstractArray` (`x`) as well as some
metadata (`md`).

# Examples

```jldoctest metadataarray_docs
julia> v = ["John", "John", "Jane", "Louise"];

julia> mdv = MetadataArray(v, groups = Dict("John" => "Treatment", "Louise" => "Placebo", "Jane" => "Placebo"))
4-element MetadataVector{String, Vector{String}, NamedTuple{(:groups,), Tuple{Dict{String, String}}}}:
 "John"
 "John"
 "Jane"
 "Louise"

julia> metadata(mdv, :groups)
Dict{String, String} with 3 entries:
  "John"   => "Treatment"
  "Jane"   => "Placebo"
  "Louise" => "Placebo"

```
"""
struct MetadataArray{T, N, P<:AbstractArray{T, N}, M<:ArrayMetadata} <: AbstractArray{T, N}
    parent::P
    metadata::M

    global _MetadataArray(p, m) = new{eltype(p), ndims(p), typeof(p), typeof(m)}(p, m)

    function MetadataArray{T, N, P, M}(p::AbstractArray, m::MDType) where {T, N, P, M}
        new{T, N, P, M}(p, m)
    end
    function MetadataArray{T, N, P}(p::AbstractArray, m::MDType) where {T, N, P}
        if isa(MDType, MetadataDict)
            mdd = m
        else
            mdd = MetadataDict(m)
        end
        MetadataArray{T, N, P, typeof(mdd)}(p, mdd)
    end
    function MetadataArray{T, N}(p::AbstractArray, m::MDType) where {T, N}
        MetadataArray{T, N, typeof(p)}(p, m)
    end
    function MetadataArray{T}(p::AbstractArray, m::MDType) where {T}
        MetadataArray{T, ndims(p)}(p, m)
    end
    MetadataArray(p::AbstractArray, m::MDType) = MetadataArray{eltype(p)}(p, m)

    #TODO: ensure `similar(mda::MetadataArray)` accounts for copy/share/drop styles
    @inline function Base.similar(mda::MetadataArray)
        p = similar(getfield(mda, :parent))
        m = copy(getfield(mda, :metadata))
        new{eltype(p), ndims(m), typeof(p), typeof(m)}(p, m)
    end
    @inline function Base.similar(mda::MetadataArray, ::Type{T}) where {T}
        p = similar(T, getfield(mda, :parent))
        m = copy(getfield(mda, :metadata))
        new{T, ndims(m), typeof(p), typeof(m)}(p, m)
    end
    function Base.similar(mda::MetadataArray, ::Type{T}, dims::Dims) where {T}
        p = similar(T, dims, getfield(mda, :parent))
        m = copy(getfield(mda, :metadata))
        new{T, ndims(m), typeof(p), typeof(m)}(p, m)
    end
end

"""
    MetadataMatrix

Shorthand for `MetadataVector{T, P<:AbstractArray{T,2}, M}`.

See also: [`MetadataArray`](@ref), [`MetadataMatrix`](@ref)
"""
const MetadataMatrix{T, P<:AbstractMatrix{T}, M} = MetadataArray{T, 2, P, M}
function MetadataMatrix{T}(p::AbstractMatrix, args...; kwargs...) where {T}
    MetadataArray{T, 2}(p, args...; kwargs...)
end
function MetadataMatrix(p::AbstractMatrix, args...; kwargs...)
    MetadataMatrix{eltype(p)}(p, args...; kwargs...)
end

"""
    MetadataVector

Shorthand for `MetadataVector{T, P<:AbstractArray{T,1}, M}`.

See also: [`MetadataArray`](@ref), [`MetadataMatrix`](@ref)
"""
const MetadataVector{T, P<:AbstractVector{T}, M} = MetadataArray{T, 1, P, M}
function MetadataVector{T}(p::AbstractVector{T}, args...; kwargs...) where {T}
    MetadataArray{T, 1}(p, args...; kwargs...)
end
function MetadataVector(p::AbstractVector, args...; kwargs...)
    MetadataVector{eltype(p)}(p, args...; kwargs...)
end

Base.parent(mda::Union{MetadataArray, MetadataUnitRange}) = getfield(mda, :parent)
function ArrayInterface.parent_type(T::Type{<:MetadataUnitRange{<:Any, <:Any, <:Any}})
    fieldtype(T, :parent)
end
function ArrayInterface.parent_type(T::Type{<:MetadataArray{<:Any, <:Any, <:Any, <:Any}})
    fieldtype(T, :parent)
end

ArrayInterface.is_forwarding_wrapper(T::Type{<:MetadataArray{<:Any, <:Any, <:Any, <:Any}}) = true
ArrayInterface.is_forwarding_wrapper(T::Type{<:MetadataUnitRange{<:Any, <:Any, <:Any}}) = true

function ArrayInterface.can_setindex(T::Type{<:MetadataArray{<:Any, <:Any, <:Any, <:Any}})
    can_setindex(fieldtype(T, :parent))
end
function ArrayInterface.can_setindex(T::Type{<:MetadataUnitRange{<:Any, <:Any, <:Any}})
    can_setindex(fieldtype(T, :parent))
end

# FIXME: this doesn't account for changes in  dim metadata indices properly
function ArrayInterface.can_change_size(T::Type{<:MetadataArray{<:Any, <:Any, <:Any, <:Any}})
    can_change_size(fieldtype(T, :parent)) && can_change_size(fieldtype(T, :metadata))
end
function ArrayInterface.can_change_size(T::Type{<:MetadataUnitRange{<:Any, <:Any, <:Any}})
    can_change_size(fieldtype(T, :parent)) && can_change_size(fieldtype(T, :metadata))
end

Base.propertynames(mda::MetadataArray) = propertynames(getfield(mda, :parent))
Base.hasproperty(mda::MetadataArray, s::Symbol) = hasproperty(getfield(mda, :parent), s)
Base.getproperty(mda::MetadataArray, s::Symbol) = getproperty(getfield(mda, :parent), s)
function Base.setproperty!(mda::MetadataArray, s::Symbol, v)
    setproperty!(getfield(mda, :parent), s, v)
end
