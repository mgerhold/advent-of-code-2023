#include <assert.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define NUM_TRAJECTORIES 300
#define MIN_TEST_AREA_BOUNDARY 200000000000000
#define MAX_TEST_AREA_BOUNDARY 400000000000000

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

    size_t count_inside_test_area = 0;

    for (size_t i = 0; i < NUM_TRAJECTORIES; ++i) {
        for (size_t j = i + 1; j < NUM_TRAJECTORIES; ++j) {
            Vec2d collision_point;
            bool const collided = are_colliding(&trajectories[i], &trajectories[j], &collision_point);
            printf("%zu, %zu: %s\n", i, j, collided ? "true" : "false");
            if (collided) {
                printf("  x = %f, y = %f\n", collision_point.x, collision_point.y);

                bool const is_inside_test_area =
                        (collision_point.x >= MIN_TEST_AREA_BOUNDARY && collision_point.x <= MAX_TEST_AREA_BOUNDARY
                         && collision_point.y >= MIN_TEST_AREA_BOUNDARY && collision_point.y <= MAX_TEST_AREA_BOUNDARY);

                if (is_inside_test_area) {
                    printf("  (is inside test area)\n");
                    ++count_inside_test_area;
                }
            }
        }
    }

    printf("\nTOTAL: %zu\n", count_inside_test_area);

    free(buffer);
}
