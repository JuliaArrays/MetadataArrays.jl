using MetadataArrays
using MetadataArrays: MetadataNode, MetadataPersistent
using Test

#=
@testset "MetadataVector" begin
    v = [1, 3, 5, 4]
    mdv = MetadataVector(v, "Numbers")
    @test length(mdv) == 4
    @test collect(mdv) == parent(mdv) == copy(mdv)
    @test metadata(mdv) == "Numbers"
    mdv[3] = 50
    @test all(mdv .== [1, 3, 50, 4])
    @test eltype(mdv) == Int
end

@testset "MetadataArray" begin
    a = [1 2; 3 4; 5 4]
    mda = MetadataArray(a, MetadataNode{MetadataPersistent}(Dict(1 => "Not at all", 10 => "A lot")))
    @test size(mda) == size(a)
    @test mda[6] == parent(mda)[6]
    @test collect(mda) == parent(mda) == a == copy(mda)
    @test metadata(mda) == Dict(1 => "Not at all", 10 => "A lot")
    mda[3, 2] = 50
    @test all(mda .== [1 2; 3 4; 5 50])
    @test parent(mda[1:2, 1]) == parent(mda)[1:2, 1]
    @test metadata(mda[1:2, 1]) == metadata(mda)
    @test metadata(rand(2)) === nothing
    @test eltype(mda) == Int

    mda = MetadataArray(rand(4), "test")
    mdar = reshape(mda, 2, 2)
    @test parent(mdar) == reshape(parent(mda), 2, 2)
    @test metadata(mdar) == metadata(mda)
    @test axes(mdar) == (Base.OneTo(2), Base.OneTo(2))
    mda2 = similar(mda, (2, 3))
    @test size(mda2) == (2, 3)
    @test metadata(mda2) == metadata(mda)

    x1 = rand(4, 4)
    m1 = MetadataArray(x1, "something")
    @test IndexStyle(m1) == IndexLinear()
    x2 = view(x1, 1:2, 1:2)
    m2 = MetadataArray(x2, "something")
    @test IndexStyle(m2) == IndexCartesian()
end
=#

a = [1 2; 3 4; 5 4]
md = (m1 =1, m2=[1, 2]);
mda = MetadataArray(a, md);

@test first(mda) == first(a)
@test last(mda) == last(a)
@test size(mda) == size(a)
@test axes(mda) == axes(a)
@test strides(mda) == strides(a)
@test length(mda) == length(a)
@test firstindex(mda) == firstindex(a)
@test lastindex(mda) == lastindex(a)
@test in(1, mda)
@test keys(mda) == keys(a)
@test !isempty(mda)
@test Base.dataids(mda) == Base.dataids(a)
@test IndexStyle(mda) == IndexStyle(a)

@test getproperty(mda, "m1") == 1
@test getproperty(mda, :m1) == 1
@test metadata(mda, "m1") == 1
@test metadata(mda, :m1) == 1
@test hasproperty(mda, "m2")
@test hasproperty(mda, :m2)
@test metadatakeys(mda) == propertynames(mda) == keys(md)
@test all(mda .== a)

#=
mxview = attach_metadata(meta)(xview)
@test @inferred(parent_type(mx)) <: typeof(x)
@test @inferred(parent_type(mxview)) <: typeof(xview)
#@test @inferred(typeof(mx)(xview, meta)) isa typeof(mx)
@test mxview.indices === xview.indices
@test ArrayInterface.defines_strides(typeof(mx))

# permutedims
@test metadata(mx') == metadata(permutedims(mx))

mvx = attach_metadata(xview, (m1 = 1, m2 = [1, 2]))
@test mvx.m1 == 1
@test mvx.m2 == [1, 2]

x = ones(4, 4);
meta = (m1 =1, m2=[1, 2]);
mx = attach_metadata(meta)(x);

@test metadata(similar(mx, eltype(mx), size(mx))) == meta
@test metadata(similar(mx, eltype(mx), axes(mx))) == meta

@test @inferred(metadata(mx)) == meta

@test @inferred(metadata(parent(mx))) === no_data
@test @inferred(has_metadata(mx))
@test @inferred(has_metadata(mx, :m1))
@test @inferred(!has_metadata(parent(mx), :m1))
@test getmeta(mx, :m1, 3) == 1
@test mx[1] == 1
@test mx[1:2] == [1, 1]
@test metadata(mx[1:2]) == metadata(mx)
@test @inferred(metadata_type(view(parent(mx), :, :))) <: Metadata.NoData
@test @inferred(metadata_type(mx)) <: NamedTuple

meta = Dict(:m1 => 1, :m2 => [1,2])
mx = attach_metadata(x, meta);
@test @inferred(parent_type(typeof(mx))) <: typeof(x)
@test @inferred(metadata(mx)) == meta
@test @inferred(has_metadata(mx))
@test @inferred(has_metadata(mx, :m1))
@test getmeta(mx, :m1, 4) == 1
@test getmeta(mx, :m4, 4) == 4
@test getmeta(ndims, x, :m4) == 2
# Currently Dict doesn't preserve order so we just check for presence of keys
@test in(:m1, propertynames(mx))
@test in(:m2, propertynames(mx))
@test mx[1] == 1
@test mx[1:2] == [1, 1]
@test @inferred(metadata(mx[1:2])) == metadata(mx)
@test @inferred(metadata_type(mx)) <: AbstractDict
# test getmeta/getmeta!

@test IndexStyle(typeof(mx)) isa IndexLinear
@test @inferred(size(mx)) == (4, 4)
@test @inferred(axes(mx)) == (1:4, 1:4)
mx.m1 = 2
@test mx.m1 == 2
=#
