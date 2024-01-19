# This file defines the generic array interface

Base.BitArray(mda::MetadataArray) = BitArray(getfield(mda, :parent))

Base.IndexStyle(T::Type{<:MetadataArray}) = IndexStyle(fieldtype(T, :parent))

Base.iterate(mda::MetadataArray) = iterate(getfield(mda, :parent))
Base.iterate(mda::MetadataArray, state) = iterate(getfield(mda, :parent), state)
Base.iterate(mda::MetadataUnitRange) = iterate(getfield(mda, :parent))
Base.iterate(mda::MetadataUnitRange, state) = iterate(getfield(mda, :parent), state)

Base.first(mda::MetadataArray) = first(getfield(mda, :parent))
Base.first(x::MetadataUnitRange) = first(getfield(x, :parent))

Base.step(mda::MetadataArray) = step(getfield(mda, :parent))

Base.last(mda::MetadataArray) = last(getfield(mda, :parent))
Base.last(x::MetadataUnitRange) = last(getfield(x, :parent))

Base.size(mda::MetadataArray) = size(getfield(mda, :parent))

Base.axes(mda::MetadataArray) = axes(getfield(mda, :parent))

Base.strides(mda::MetadataArray) = strides(getfield(mda, :parent))

Base.length(mda::MetadataArray) = length(getfield(mda, :parent))
Base.length(mda::MetadataUnitRange) = length(getfield(mda, :parent))

Base.firstindex(mda::MetadataArray) = firstindex(getfield(mda, :parent))
Base.firstindex(mda::MetadataUnitRange) = firstindex(getfield(mda, :parent))

Base.lastindex(mda::MetadataArray) = lastindex(getfield(mda, :parent))
Base.lastindex(mda::MetadataUnitRange) = lastindex(getfield(mda, :parent))

Base.pointer(mda::MetadataArray) = pointer(getfield(mda, :parent))
Base.pointer(mda::MetadataArray, i::Integer) = pointer(getfield(mda, :parent), i)

Base.keys(mda::MetadataArray) = keys(getfield(mda, :parent))

Base.@propagate_inbounds function Base.isassigned(mda::MetadataArray, i::Integer...)
    isassigned(getfield(mda, :parent), i...)
end

function Base.dataids(mda::MetadataArray)
    (Base.dataids(getfield(mda, :parent))..., Base.dataids(getfield(mda, :metadata))...)
end

function Base.transpose(mda::MetadataArray)
    p = transpose(getfield(mda, :parent))
    m = permute_dimsmetadata(mda)
    _MetadataArray(p, m)
end
function Base.adjoint(mda::MetadataArray)
    p = adjoint(getfield(mda, :parent))
    m = permute_dimsmetadata(mda)
    _MetadataArray(p, m)
end
function Base.permutedims(mda::MetadataArray)
    p = permutedims(getfield(mda, :parent))
    m = permute_dimsmetadata(mda)
    _MetadataArray(p, m)
end
function Base.permutedims(mda::MetadataArray, perm)
    p = permutedims(getfield(mda, :parent), perm)
    m = permute_dimsmetadata(mda, perm)
    _MetadataArray(p, m)
end

