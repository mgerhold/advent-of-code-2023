#include <assert.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define NUM_TRAJECTORIES 5

[[nodiscard]] char* read_file(char const* const filename) {
    FILE* file = fopen(filename, "rb");
    if (file == nullptr) {
        return nullptr;
    }

    fseek(file, 0, SEEK_END);
    long const fsize = ftell(file);
    fseek(file, 0, SEEK_SET);

    if (fsize == LONG_MAX) {
        fclose(file);
        return nullptr;
    }

    char* buffer = malloc(fsize + 1);
    if (buffer == nullptr) {
        fclose(file);
        return nullptr;
    }
    fread(buffer, sizeof(char), fsize, file);
    fclose(file);

    buffer[fsize] = '\0';
    return buffer;
}

typedef struct {
    int64_t x;
    int64_t y;
    int64_t z;
} Vec3i;

typedef struct {
    Vec3i position;
    Vec3i velocity;
} Trajectory;

typedef struct {
    double x;
    double y;
} Vec2d;

[[nodiscard]] Trajectory read_trajectory(char const* const string) {
    Vec3i position;
    Vec3i velocity;
    int const result =
            sscanf(string,
                   "%" SCNd64 ", %" SCNd64 ", %" SCNd64 " @ %" SCNd64 ", %" SCNd64 ", %" SCNd64,
                   &position.x,
                   &position.y,
                   &position.z,
                   &velocity.x,
                   &velocity.y,
                   &velocity.z);
    if (result != 6) {
        printf("error reading trajectory from '%s'\n", string);
        exit(EXIT_FAILURE);
    }
    return (Trajectory){ position, velocity };
}

// clang-format off
[[nodiscard]] bool are_colliding(
    Trajectory const* const lhs,
    Trajectory const* const rhs,
    Vec2d* const point
) { // clang-format on
    *point = (Vec2d){ 0.0, 0.0 };

    int64_t const denominator = -rhs->velocity.x * lhs->velocity.y + lhs->velocity.x * rhs->velocity.y;
    if (denominator == 0) {
        return false;
    }

    int64_t const numerator = lhs->velocity.y * (rhs->position.x - lhs->position.x)
                              - lhs->velocity.x * (rhs->position.y - lhs->position.y);

    double const r = (double) numerator / (double) denominator;

    if (r < 0.0) {
        return false;
    }

    assert(lhs->velocity.x != 0);
    double const t = ((double) rhs->position.x - (double) lhs->position.x + (double) rhs->velocity.x * r)
                     / (double) lhs->velocity.x;

    if (t < 0.0) {
        return false;
    }

    *point = (Vec2d){
        rhs->position.x + r * rhs->velocity.x,
        rhs->position.y + r * rhs->velocity.y,
    };

    return true;
}

int main() {
    char* const buffer = read_file("data.txt");
    if (buffer == nullptr) {
        printf("Error reading from file.\n");
        return EXIT_FAILURE;
    }

    printf("%s\n", buffer);
    Trajectory trajectories[NUM_TRAJECTORIES];
    char const* current = buffer;
    for (size_t i = 0; i < NUM_TRAJECTORIES; ++i) {
        trajectories[i] = read_trajectory(current);
        while (*current != '\n' && *current != '\0') {
            ++current;
        }
        if (*current == '\n') {
            ++current;
        }
        if (i < 4 && *current == '\0') {
            printf("unable to read all trajectories");
            exit(EXIT_FAILURE);
        }
    }

    for (size_t i = 0; i < 3; ++i) {
        printf("r_x+t_%zu*v_x=%"PRId64"+t_%zu*%"PRId64",\n", i, trajectories[i].position.x, i, trajectories[i].velocity.x);
        printf("r_y+t_%zu*v_y=%"PRId64"+t_%zu*%"PRId64",\n", i, trajectories[i].position.y, i, trajectories[i].velocity.y);
        printf("r_z+t_%zu*v_z=%"PRId64"+t_%zu*%"PRId64",\n", i, trajectories[i].position.z, i, trajectories[i].velocity.z);
    }

    free(buffer);
}
