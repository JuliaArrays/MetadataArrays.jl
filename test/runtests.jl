using MetadataArrays
@static if VERSION < v"0.7.0-DEV.2005"
    using Base.Test
else
    using Test
end

@testset "MetadataVector" begin
    v = [1, 3, 5, 4]
    sv = MetadataVector(v, "Numbers")
    @test length(sv) == 4
    @test collect(sv) == parent(sv) == copy(sv)
    @test metadata(sv) == "Numbers"
    sv[3] = 50
    @test all(sv .== [1, 3, 50, 4])
    @test eltype(sv) == Int
end

@testset "MetadataArray" begin
    v = [1 2; 3 4; 5 4]
    sv = MetadataArray(v, Dict(1 => "Not at all", 10 => "A lot"))
    @test size(sv) == size(v)
    @test collect(sv) == parent(sv) == v == copy(sv)
    @test metadata(sv) == Dict(1 => "Not at all", 10 => "A lot")
    sv[3, 2] = 50
    @test all(sv .== [1 2; 3 4; 5 50])
    @test eltype(sv) == Int
end
