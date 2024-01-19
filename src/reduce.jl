
function Base.mapreduce(f, op, mda::MetadataArray; dims=:, kwargs...)
    p = mapreduce(f, op, getfield(mda, :parent); dims=dims, kwargs...)
    m = reduce_dimsmetadata(mda, dims)
    _MetadataArray(p, m)
end
function Statistics.mean(mda::MetadataArray; dims=:, kwargs...)
    p = Statistics.mean(getfield(mda, :parent); dims=dims, kwargs...)
    m = reduce_dimsmetadata(mda, dims)
    _MetadataArray(p, m)
end
function Statistics.std(mda::MetadataArray; dims=:, kwargs...)
    p = Statistics.std(getfield(mda, :parent); dims=dims, kwargs...)
    m = reduce_dimsmetadata(mda, dims)
    _MetadataArray(p, m)
end
function Statistics.var(mda::MetadataArray; dims=:, kwargs...)
    p = Statistics.var(getfield(mda, :parent); dims=dims, kwargs...)
    m = reduce_dimsmetadata(mda, dims)
    _MetadataArray(p, m)
end
function Statistics.median(mda::MetadataArray; dims=:, kwargs...)
    p = Statistics.median(getfield(mda, :parent); dims=dims, kwargs...)
    m = reduce_dimsmetadata(mda, dims)
    _MetadataArray(p, m)
end
