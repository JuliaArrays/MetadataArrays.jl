
Base.@propagate_inbounds function Base.getindex(mdu::MetadataUnitRange, i::Integer)
    getindex(getfield(mdu, :parent), i)
end
Base.@propagate_inbounds function Base.getindex(mdu::MetadataUnitRange, i)
    propagate_metadata(mdu, getindex(getfield(mdu, :parent), i))
end

Base.@propagate_inbounds function Base.getindex(
    mdu::MetadataUnitRange,
    s::StepRange{T}
) where T<:Integer
    propagate_metadata(mdu, getindex(getfield(mdu, :parent), s))
end
Base.@propagate_inbounds function Base.getindex(
    mdu::MetadataUnitRange,
    s::AbstractUnitRange{T}
) where {T<:Integer}
    propagate_metadata(mdu, getindex(getfield(mdu, :parent), s))
end

Base.getindex(mdu::MetadataUnitRange, ::Colon) = copy(mdu)

Base.copy(mdu::MetadataUnitRange) = copy_metadata(mdu, copy(getfield(mdu, :parent)))

Base.@propagate_inbounds function Base.getindex(mda::MetadataArray, i::Int...)
    getfield(mda, :parent)[i...]
end
Base.@propagate_inbounds function Base.getindex(mda::MetadataArray, inds...)
    MetadataArray(getfield(mda, :parent)[inds...], getfield(mda, :metadata))
end
Base.@propagate_inbounds function Base.view(mda::MetadataArray, inds...)
    MetadataArray(view(getfield(mda, :parent), inds...), getfield(mda, :metadata))
end
Base.@propagate_inbounds function Base.setindex!(mda::MetadataArray, val, inds...)
    setindex!(getfield(mda, :parent),val, inds...)
end
