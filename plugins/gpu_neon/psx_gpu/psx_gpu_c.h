u32 fixed_reciprocal(u32 denominator, u32 *_shift)
{
  u32 shift = __builtin_clz(denominator);
  u32 denominator_normalized = denominator << shift;

  double numerator = (1ULL << 62) + denominator_normalized;
  double numerator_b;

  double denominator_normalized_dp_b;
  u64 denominator_normalized_dp_u64;

  u32 reciprocal;
  double reciprocal_dp;

  u64 numerator_u64 = (denominator_normalized >> 10) |
   ((u64)(62 + 1023) << 52);
  *((u64 *)(&numerator_b)) = numerator_u64;

  denominator_normalized_dp_u64 =
   (u64)(denominator_normalized << 21) |
   ((u64)((denominator_normalized >> 11) + ((1022 + 31) << 20)) << 32);
  *((u64 *)(&denominator_normalized_dp_b)) = denominator_normalized_dp_u64;

  // Implement with a DP divide
  reciprocal_dp = numerator / denominator_normalized_dp_b;
  reciprocal = reciprocal_dp;

  if(reciprocal == 0x80000001)
    reciprocal = 0x80000000;

  *_shift = 62 - shift;
  return reciprocal;
}

double reciprocal_estimate(double a)
{
  int q, s;
  double r;

  q = (int)(a * 512.0);
  /* a in units of 1/512 rounded down */
  r = 1.0 / (((double)q + 0.5) / 512.0); /* reciprocal r */
  s = (int)(256.0 * r + 0.5);

  /* r in units of 1/256 rounded to nearest */
  
  return (double)s / 256.0;
}

u32 reciprocal_estimate_u32(u32 value)
{
  u64 dp_value_u64;
  volatile double dp_value;
  volatile u64 *dp_value_ptr = (volatile u64 *)&dp_value;

  if((value >> 31) == 0)
    return 0xFFFFFFFF;

  dp_value_u64 = (0x3FEULL << (31 + 21)) | ((u64)(value & 0x7FFFFFFF) << 21);

  *dp_value_ptr = dp_value_u64;

  dp_value = reciprocal_estimate(dp_value);
  dp_value_u64 = *dp_value_ptr;

  return (0x80000000 | ((dp_value_u64 >> 21) & 0x7FFFFFFF));
}

u32 fixed_reciprocal_nr(u32 value, u32 *_shift)
{
  u32 shift = __builtin_clz(value);
  u32 value_normalized = value << shift;

  *_shift = 62 - shift;

  value_normalized -= 2;

  u32 reciprocal_normalized = reciprocal_estimate_u32(value_normalized) >> 1;

  u32 temp = -(((u64)value_normalized * (u32)reciprocal_normalized) >> 31);
  reciprocal_normalized = (((u64)reciprocal_normalized * temp) >> 31);
  temp = -(((u64)value_normalized * (u32)reciprocal_normalized) >> 31);
  reciprocal_normalized = (((u64)reciprocal_normalized * temp) >> 31);
  temp = -(((u64)value_normalized * (u32)reciprocal_normalized) >> 31);
  reciprocal_normalized = (((u64)reciprocal_normalized * temp) >> 31);

  return reciprocal_normalized;
}


void update_texture_4bpp_cache(psx_gpu_struct *psx_gpu)
{
  u32 current_texture_page = psx_gpu->current_texture_page;
  u8 *texture_page_ptr = psx_gpu->texture_page_base;
  u16 *vram_ptr = psx_gpu->vram_ptr;

  u32 texel_block;
  u32 tile_x, tile_y;
  u32 sub_x, sub_y;

  vram_ptr += (current_texture_page >> 4) * 256 * 1024;
  vram_ptr += (current_texture_page & 0xF) * 64;

  texture_cache_loads++;

  tile_y = 16;
  tile_x = 16;
  sub_x = 4;
  sub_y = 16;

  psx_gpu->dirty_textures_4bpp_mask &= ~(psx_gpu->current_texture_mask);

  while(tile_y)
  {
    while(tile_x)
    {
      while(sub_y)
      {
        while(sub_x)
        {
          texel_block = *vram_ptr;

          texture_page_ptr[0] = texel_block & 0xF;
          texture_page_ptr[1] = (texel_block >> 4) & 0xF;
          texture_page_ptr[2] = (texel_block >> 8) & 0xF;
          texture_page_ptr[3] = texel_block >> 12;
          
          vram_ptr++;
          texture_page_ptr += 4;

          sub_x--;          
        }

        vram_ptr -= 4;
        sub_x = 4;

        sub_y--;
        vram_ptr += 1024;
      }

      sub_y = 16;

      vram_ptr -= (1024 * 16) - 4;
      tile_x--;
    }

    tile_x = 16;

    vram_ptr += (16 * 1024) - (4 * 16);
    tile_y--;
  }
}

void update_texture_8bpp_cache_slice(psx_gpu_struct *psx_gpu,
 u32 texture_page)
{
  u16 *texture_page_ptr = psx_gpu->texture_page_base;
  u16 *vram_ptr = psx_gpu->vram_ptr;

  u32 tile_x, tile_y;
  u32 sub_y;

  vec_8x16u texels;

  texture_cache_loads++;

  vram_ptr += (texture_page >> 4) * 256 * 1024;
  vram_ptr += (texture_page & 0xF) * 64;

  if((texture_page ^ psx_gpu->current_texture_page) & 0x1)
    texture_page_ptr += (8 * 16) * 8;

  tile_x = 8;
  tile_y = 16;

  sub_y = 16;

  while(tile_y)
  {
    while(tile_x)
    {
      while(sub_y)
      {
        load_128b(texels, vram_ptr);
        store_128b(texels, texture_page_ptr);

        texture_page_ptr += 8;
        vram_ptr += 1024;

        sub_y--;
      }

      sub_y = 16;

      vram_ptr -= (1024 * 16);
      vram_ptr += 8;

      tile_x--;
    }

    tile_x = 8;

    vram_ptr -= (8 * 8);
    vram_ptr += (16 * 1024);

    texture_page_ptr += (8 * 16) * 8;
    tile_y--;
  }
}

#define setup_gradient_calculation_input(set, vertex)                          \
  /* First type is:  uvrg bxxx xxxx                                          */\
  /* Second type is: yyyy ybyy uvrg                                          */\
  /* Since x_a and y_c are the same the same variable is used for both.      */\
  x##set##_a_y##set##_c.e[0] = vertex->u;                                      \
  x##set##_a_y##set##_c.e[1] = vertex->v;                                      \
  x##set##_a_y##set##_c.e[2] = vertex->r;                                      \
  x##set##_a_y##set##_c.e[3] = vertex->g;                                      \
  dup_4x16b(x##set##_b, vertex->x);                                            \
  dup_4x16b(x##set##_c, vertex->x);                                            \
  dup_4x16b(y##set##_a, vertex->y);                                            \
  dup_4x16b(y##set##_b, vertex->y);                                            \
  x##set##_b.e[0] = vertex->b;                                                 \
  y##set##_b.e[1] = vertex->b                                                  \
  

void compute_all_gradients(psx_gpu_struct *psx_gpu, vertex_struct *a,
 vertex_struct *b, vertex_struct *c)
{
  u32 triangle_area = psx_gpu->triangle_area;
  u32 winding_mask_scalar;

  u32 triangle_area_shift;
  u64 triangle_area_reciprocal =
   fixed_reciprocal(triangle_area, &triangle_area_shift);
  triangle_area_shift = -(triangle_area_shift - FIXED_BITS);

  // ((x1 - x0) * (y2 - y1)) - ((x2 - x1) * (y1 - y0)) =
  // ( d0       *  d1      ) - ( d2       *  d3      ) =
  // ( m0                  ) - ( m1                  ) = gradient

  // This is split to do 12 elements at a time over three sets: a, b, and c.
  // Technically we only need to do 10 elements (uvrgb_x and uvrgb_y), so
  // two of the slots are unused.

  // Inputs are all 16-bit signed. The m0/m1 results are 32-bit signed, as
  // is g.

  vec_4x16s x0_a_y0_c, x0_b, x0_c;
  vec_4x16s y0_a, y0_b;
  vec_4x16s x1_a_y1_c, x1_b, x1_c;
  vec_4x16s y1_a, y1_b;
  vec_4x16s x2_a_y2_c, x2_b, x2_c;
  vec_4x16s y2_a, y2_b;

  vec_4x32u uvrg_base;
  vec_4x32u b_base;
  vec_4x32u uvrgb_phase;

  vec_4x16s d0_a_d3_c, d0_b, d0_c;
  vec_4x16s d1_a, d1_b, d1_c_d2_a;
  vec_4x16s d2_b, d2_c;
  vec_4x16s d3_a, d3_b;

  vec_4x32s m0_a, m0_b, m0_c;
  vec_4x32s m1_a, m1_b, m1_c;

  vec_4x32u gradient_area_a, gradient_area_c;
  vec_2x32u gradient_area_b;  

  vec_4x32u gradient_area_sign_a, gradient_area_sign_c;
  vec_2x32u gradient_area_sign_b;
  vec_4x32u winding_mask;

  vec_2x64u gradient_wide_a0, gradient_wide_a1;
  vec_2x64u gradient_wide_c0, gradient_wide_c1;
  vec_2x64u gradient_wide_b;

  vec_4x32u gradient_a, gradient_c;
  vec_2x32u gradient_b;
  vec_16x8s gradient_shift;

  setup_gradient_calculation_input(0, a);
  setup_gradient_calculation_input(1, b);
  setup_gradient_calculation_input(2, c);

  dup_4x32b(uvrgb_phase, psx_gpu->uvrgb_phase);
  shl_long_4x16b(uvrg_base, x0_a_y0_c, 16);
  shl_long_4x16b(b_base, x0_b, 16);

  add_4x32b(uvrg_base, uvrg_base, uvrgb_phase);
  add_4x32b(b_base, b_base, uvrgb_phase);

  // Can probably pair these, but it'll require careful register allocation
  sub_4x16b(d0_a_d3_c, x1_a_y1_c, x0_a_y0_c);
  sub_4x16b(d1_c_d2_a, x2_a_y2_c, x1_a_y1_c);

  sub_4x16b(d0_b, x1_b, x0_b);
  sub_4x16b(d0_c, x1_c, x0_c);

  sub_4x16b(d1_a, y2_a, y1_a);
  sub_4x16b(d1_b, y2_b, y1_b);

  sub_4x16b(d2_b, x2_b, x1_b);
  sub_4x16b(d2_c, x2_c, x1_c);

  sub_4x16b(d3_a, y1_a, y0_a);
  sub_4x16b(d3_b, y1_b, y0_b);

  mul_long_4x16b(m0_a, d0_a_d3_c, d1_a);
  mul_long_4x16b(m0_b, d0_b, d1_b);
  mul_long_4x16b(m0_c, d0_c, d1_c_d2_a);

  mul_long_4x16b(m1_a, d1_c_d2_a, d3_a);
  mul_long_4x16b(m1_b, d2_b, d3_b);
  mul_long_4x16b(m1_c, d2_c, d0_a_d3_c);

  sub_4x32b(gradient_area_a, m0_a, m1_a);
  sub_2x32b(gradient_area_b, m0_b.low, m1_b.low);
  sub_4x32b(gradient_area_c, m0_c, m1_c);

  cmpltz_4x32b(gradient_area_sign_a, gradient_area_a);
  cmpltz_2x32b(gradient_area_sign_b, gradient_area_b);
  cmpltz_4x32b(gradient_area_sign_c, gradient_area_c);

  abs_4x32b(gradient_area_a, gradient_area_a);
  abs_2x32b(gradient_area_b, gradient_area_b);
  abs_4x32b(gradient_area_c, gradient_area_c);

  winding_mask_scalar = -psx_gpu->triangle_winding;

  dup_4x32b(winding_mask, winding_mask_scalar);
  eor_4x32b(gradient_area_sign_a, gradient_area_sign_a, winding_mask);
  eor_2x32b(gradient_area_sign_b, gradient_area_sign_b, winding_mask);
  eor_4x32b(gradient_area_sign_c, gradient_area_sign_c, winding_mask);

  mul_scalar_long_2x32b(gradient_wide_a0, 
   vector_cast(vec_2x32s, gradient_area_a.low), 
   (s64)triangle_area_reciprocal);
  mul_scalar_long_2x32b(gradient_wide_a1,
   vector_cast(vec_2x32s, gradient_area_a.high),
   (s64)triangle_area_reciprocal);
  mul_scalar_long_2x32b(gradient_wide_b, 
   vector_cast(vec_2x32s, gradient_area_b),
   (s64)triangle_area_reciprocal);
  mul_scalar_long_2x32b(gradient_wide_c0, 
   vector_cast(vec_2x32s, gradient_area_c.low),
   (s64)triangle_area_reciprocal);
  mul_scalar_long_2x32b(gradient_wide_c1, 
   vector_cast(vec_2x32s, gradient_area_c.high),
   (s64)triangle_area_reciprocal);

  dup_16x8b(gradient_shift, triangle_area_shift);
  shl_reg_2x64b(gradient_wide_a0, gradient_wide_a0,
   vector_cast(vec_2x64u, gradient_shift));
  shl_reg_2x64b(gradient_wide_a1, gradient_wide_a1,
   vector_cast(vec_2x64u, gradient_shift));
  shl_reg_2x64b(gradient_wide_b, gradient_wide_b,
   vector_cast(vec_2x64u, gradient_shift));
  shl_reg_2x64b(gradient_wide_c0, gradient_wide_c0,
   vector_cast(vec_2x64u, gradient_shift));
  shl_reg_2x64b(gradient_wide_c1, gradient_wide_c1,
   vector_cast(vec_2x64u, gradient_shift));

  mov_narrow_2x64b(gradient_a.low, gradient_wide_a0);
  mov_narrow_2x64b(gradient_a.high, gradient_wide_a1);
  mov_narrow_2x64b(gradient_b, gradient_wide_b);
  mov_narrow_2x64b(gradient_c.low, gradient_wide_c0);
  mov_narrow_2x64b(gradient_c.high, gradient_wide_c1);

  shl_4x32b(gradient_a, gradient_a, 4);
  shl_2x32b(gradient_b, gradient_b, 4);
  shl_4x32b(gradient_c, gradient_c, 4);

  eor_4x32b(gradient_a, gradient_a, gradient_area_sign_a);
  eor_2x32b(gradient_b, gradient_b, gradient_area_sign_b);
  eor_4x32b(gradient_c, gradient_c, gradient_area_sign_c);

  sub_4x32b(gradient_a, gradient_a, gradient_area_sign_a);
  sub_2x32b(gradient_b, gradient_b, gradient_area_sign_b);
  sub_4x32b(gradient_c, gradient_c, gradient_area_sign_c);

  u32 left_adjust = a->x;
  mls_scalar_4x32b(uvrg_base, gradient_a, left_adjust);
  mls_scalar_2x32b(b_base.low, gradient_b, left_adjust);

  vec_4x32u uvrg_dx2;
  vec_2x32u b_dx2;

  vec_4x32u uvrg_dx3;
  vec_2x32u b_dx3;

  vec_4x32u zero;

  eor_4x32b(zero, zero, zero);
  add_4x32b(uvrg_dx2, gradient_a, gradient_a);
  add_2x32b(b_dx2, gradient_b, gradient_b);
  add_4x32b(uvrg_dx3, gradient_a, uvrg_dx2);
  add_2x32b(b_dx3, gradient_b, b_dx2);

  // Can be done with vst4, assuming that the zero, dx, dx2, and dx3 are
  // lined up properly
  psx_gpu->u_block_span.e[0] = zero.e[0];
  psx_gpu->u_block_span.e[1] = gradient_a.e[0];
  psx_gpu->u_block_span.e[2] = uvrg_dx2.e[0];
  psx_gpu->u_block_span.e[3] = uvrg_dx3.e[0];

  psx_gpu->v_block_span.e[0] = zero.e[1];
  psx_gpu->v_block_span.e[1] = gradient_a.e[1];
  psx_gpu->v_block_span.e[2] = uvrg_dx2.e[1];
  psx_gpu->v_block_span.e[3] = uvrg_dx3.e[1];

  psx_gpu->r_block_span.e[0] = zero.e[2];
  psx_gpu->r_block_span.e[1] = gradient_a.e[2];
  psx_gpu->r_block_span.e[2] = uvrg_dx2.e[2];
  psx_gpu->r_block_span.e[3] = uvrg_dx3.e[2];

  psx_gpu->g_block_span.e[0] = zero.e[3];
  psx_gpu->g_block_span.e[1] = gradient_a.e[3];
  psx_gpu->g_block_span.e[2] = uvrg_dx2.e[3];
  psx_gpu->g_block_span.e[3] = uvrg_dx3.e[3];

  psx_gpu->b_block_span.e[0] = zero.e[0];
  psx_gpu->b_block_span.e[1] = gradient_b.e[0];
  psx_gpu->b_block_span.e[2] = b_dx2.e[0];
  psx_gpu->b_block_span.e[3] = b_dx3.e[0];

  psx_gpu->uvrg = uvrg_base;
  psx_gpu->b = b_base.e[0];

  psx_gpu->uvrg_dx = gradient_a;
  psx_gpu->uvrg_dy = gradient_c;
  psx_gpu->b_dy = gradient_b.e[1];
}


void setup_spans_up_left(psx_gpu_struct *psx_gpu, vertex_struct *v_a,
 vertex_struct *v_b, vertex_struct *v_c)
{
  setup_spans_up_up(left, right);
}

void setup_spans_up_right(psx_gpu_struct *psx_gpu, vertex_struct *v_a,
 vertex_struct *v_b, vertex_struct *v_c)
{
  setup_spans_up_up(right, left);
}

#define setup_spans_down_down(minor, major)                                    \
  setup_spans_prologue(yes);                                                   \
  s32 height_minor_a = y_b - y_a;                                              \
  s32 height_minor_b = y_c - y_b;                                              \
  s32 height = y_c - y_a;                                                      \
                                                                               \
  dup_2x32b(x_starts, x_a);                                                    \
  x_ends.e[0] = x_c;                                                           \
  x_ends.e[1] = x_b;                                                           \
                                                                               \
  compute_edge_delta_x3(x_b, height, height_minor_a);                          \
  setup_spans_down(index_##major, index_##minor, minor, yes)                   \

void setup_spans_down_left(psx_gpu_struct *psx_gpu, vertex_struct *v_a,
 vertex_struct *v_b, vertex_struct *v_c)
{
  setup_spans_down_down(left, right);
}

void setup_spans_down_right(psx_gpu_struct *psx_gpu, vertex_struct *v_a,
 vertex_struct *v_b, vertex_struct *v_c)
{
  setup_spans_down_down(right, left);
}

#define setup_spans_up_flat()                                                  \
  s32 height = y_a - y_c;                                                      \
                                                                               \
  flat_triangles++;                                                            \
  compute_edge_delta_x2();                                                     \
  setup_spans_up(index_left, index_right, none, no)                            \

void setup_spans_up_a(psx_gpu_struct *psx_gpu, vertex_struct *v_a,
 vertex_struct *v_b, vertex_struct *v_c)
{
  setup_spans_prologue(no);
  x_starts.e[0] = x_a;
  x_starts.e[1] = x_b;
  dup_2x32b(x_ends, x_c);

  setup_spans_up_flat();
}

void setup_spans_up_b(psx_gpu_struct *psx_gpu, vertex_struct *v_a,
 vertex_struct *v_b, vertex_struct *v_c)
{
  setup_spans_prologue(no);
  dup_2x32b(x_starts, x_a);
  x_ends.e[0] = x_b;
  x_ends.e[1] = x_c;

  setup_spans_up_flat();
}

#define setup_spans_down_flat()                                                \
  s32 height = y_c - y_a;                                                      \
                                                                               \
  flat_triangles++;                                                            \
  compute_edge_delta_x2();                                                     \
  setup_spans_down(index_left, index_right, none, no)                          \

void setup_spans_down_a(psx_gpu_struct *psx_gpu, vertex_struct *v_a,
 vertex_struct *v_b, vertex_struct *v_c)
{
  setup_spans_prologue(no);
  x_starts.e[0] = x_a;
  x_starts.e[1] = x_b;
  dup_2x32b(x_ends, x_c);

  setup_spans_down_flat();
}

void setup_spans_down_b(psx_gpu_struct *psx_gpu, vertex_struct *v_a,
 vertex_struct *v_b, vertex_struct *v_c)
{
  setup_spans_prologue(no);
  dup_2x32b(x_starts, x_a);
  x_ends.e[0] = x_b;
  x_ends.e[1] = x_c;

  setup_spans_down_flat();
}

void setup_spans_up_down(psx_gpu_struct *psx_gpu, vertex_struct *v_a,
 vertex_struct *v_b, vertex_struct *v_c)
{
  setup_spans_prologue(no);

  s32 y_b = v_b->y;
  s64 edge_alt;
  s32 edge_dx_dy_alt;
  u32 edge_shift_alt;

  s32 middle_y = y_a;
  s32 height_minor_a = y_a - y_b;
  s32 height_minor_b = y_c - y_a;
  s32 height_major = y_c - y_b;

  vec_2x64s edges_xy_b;
  vec_2x32s edges_dx_dy_b;
  vec_2x32u edge_shifts_b;

  vec_2x32s height_increment;

  x_starts.e[0] = x_a;
  x_starts.e[1] = x_c;
  dup_2x32b(x_ends, x_b);

  compute_edge_delta_x3(x_a, height_minor_a, height_major);

  height_increment.e[0] = 0;
  height_increment.e[1] = height_minor_b;

  mla_long_2x32b(edges_xy, edges_dx_dy, height_increment);

  edges_xy_b.e[0] = edge_alt;
  edges_xy_b.e[1] = edges_xy.e[1];

  edge_shifts_b = edge_shifts;
  edge_shifts_b.e[0] = edge_shift_alt;

  neg_2x32b(edges_dx_dy_b, edges_dx_dy);
  edges_dx_dy_b.e[0] = edge_dx_dy_alt;
  
  y_a--;

  if(y_b < psx_gpu->viewport_start_y)
    height_minor_a -= psx_gpu->viewport_start_y - y_b;

  clip = y_a - psx_gpu->viewport_end_y;
  if(clip > 0)
  {
    height_minor_a -= clip;
    y_a -= clip;
    setup_spans_clip(decrement, no);
  }

  setup_spans_prologue_b();

  if(height_minor_a > 0)
  {
    y_x4.e[0] = y_a;
    y_x4.e[1] = y_a - 1;
    y_x4.e[2] = y_a - 2;
    y_x4.e[3] = y_a - 3;
    add_wide_2x32b(edges_xy, edges_xy, edges_dx_dy);
    setup_spans_adjust_edges_alternate_no(index_left, index_right);
    setup_spans_adjust_interpolants_up();

    psx_gpu->num_spans = height_minor_a;
    while(height_minor_a > 0)
    {
      setup_spans_set_x4(none, up, no);
      height_minor_a -= 4;
    }

    span_edge_data += height_minor_a;
    span_uvrg_offset += height_minor_a;
    span_b_offset += height_minor_a;
  }
  
  edges_xy = edges_xy_b;
  edges_dx_dy = edges_dx_dy_b;
  edge_shifts = edge_shifts_b;

  uvrg = psx_gpu->uvrg;
  b = psx_gpu->b;

  y_a = middle_y;

  if(y_c > psx_gpu->viewport_end_y)
    height_minor_b -= y_c - psx_gpu->viewport_end_y - 1;

  clip = psx_gpu->viewport_start_y - y_a;
  if(clip > 0)
  {
    height_minor_b -= clip;
    y_a += clip;
    setup_spans_clip(increment, no);
  }

  if(height_minor_b > 0)
  {
    y_x4.e[0] = y_a;
    y_x4.e[1] = y_a + 1;
    y_x4.e[2] = y_a + 2;
    y_x4.e[3] = y_a + 3;
    setup_spans_adjust_edges_alternate_no(index_left, index_right);

    // FIXME: overflow corner case
    if(psx_gpu->num_spans + height_minor_b == MAX_SPANS)
      height_minor_b &= ~3;

    psx_gpu->num_spans += height_minor_b;
    while(height_minor_b > 0)
    {
      setup_spans_set_x4(none, down, no);
      height_minor_b -= 4;
    }
  }

  left_split_triangles++;
}

setup_blocks_builder(shaded, textured, dithered, swizzled, indirect);
setup_blocks_builder(shaded, textured, dithered, unswizzled, indirect);

setup_blocks_builder(unshaded, textured, dithered, unswizzled, indirect);
setup_blocks_builder(unshaded, textured, dithered, swizzled, indirect);

setup_blocks_builder(shaded, untextured, undithered, unswizzled, indirect);
setup_blocks_builder(shaded, untextured, dithered, unswizzled, indirect);
setup_blocks_builder(shaded, untextured, undithered, unswizzled, direct);
setup_blocks_builder(shaded, untextured, dithered, unswizzled, direct);

setup_blocks_builder(unshaded, untextured, undithered, unswizzled, indirect);
setup_blocks_builder(unshaded, untextured, undithered, unswizzled, direct);

void texture_blocks_untextured(psx_gpu_struct *psx_gpu)
{
  if(psx_gpu->primitive_type != PRIMITIVE_TYPE_SPRITE)
    texel_blocks_untextured += psx_gpu->num_blocks;
}

void texture_blocks_4bpp(psx_gpu_struct *psx_gpu)
{
  block_struct *block = psx_gpu->blocks;
  u32 num_blocks = psx_gpu->num_blocks;
  texel_blocks_4bpp += num_blocks;

  vec_8x8u texels_low;
  vec_8x8u texels_high;
  vec_8x8u texels;
  vec_8x16u pixels;

  vec_8x16u clut_a;
  vec_8x16u clut_b;
  vec_16x8u clut_low;
  vec_16x8u clut_high;

  u8 *texture_ptr_8bpp = psx_gpu->texture_page_ptr;
  u16 *clut_ptr = psx_gpu->clut_ptr;

  // Can be done with one deinterleaving load on NEON
  load_8x16b(clut_a, clut_ptr);
  load_8x16b(clut_b, clut_ptr + 8);
  unzip_16x8b(clut_low, clut_high, clut_a, clut_b);

  if(psx_gpu->current_texture_mask & psx_gpu->dirty_textures_4bpp_mask)
    update_texture_4bpp_cache(psx_gpu);

  while(num_blocks)
  {
    texels.e[0] = texture_ptr_8bpp[block->uv.e[0]];
    texels.e[1] = texture_ptr_8bpp[block->uv.e[1]];
    texels.e[2] = texture_ptr_8bpp[block->uv.e[2]];
    texels.e[3] = texture_ptr_8bpp[block->uv.e[3]];
    texels.e[4] = texture_ptr_8bpp[block->uv.e[4]];
    texels.e[5] = texture_ptr_8bpp[block->uv.e[5]];
    texels.e[6] = texture_ptr_8bpp[block->uv.e[6]];
    texels.e[7] = texture_ptr_8bpp[block->uv.e[7]];

    tbl_16(texels_low, texels, clut_low);
    tbl_16(texels_high, texels, clut_high);

    // Can be done with an interleaving store on NEON
    zip_8x16b(pixels, texels_low, texels_high);

    block->texels = pixels;

    num_blocks--;
    block++;
  }
}

void texture_blocks_8bpp(psx_gpu_struct *psx_gpu)
{
  block_struct *block = psx_gpu->blocks;
  u32 num_blocks = psx_gpu->num_blocks;

  texel_blocks_8bpp += num_blocks;

  if(psx_gpu->current_texture_mask & psx_gpu->dirty_textures_8bpp_mask)
    update_texture_8bpp_cache(psx_gpu);

  vec_8x16u texels;
  u8 *texture_ptr_8bpp = psx_gpu->texture_page_ptr;

  u32 texel;
  u32 offset;
  u32 i;

  while(num_blocks)
  {
    for(i = 0; i < 8; i++)
    {
      offset = block->uv.e[i];

      texel = texture_ptr_8bpp[offset];
      texels.e[i] = psx_gpu->clut_ptr[texel];
    }

    block->texels = texels;

    num_blocks--;
    block++;
  }
}

void texture_blocks_16bpp(psx_gpu_struct *psx_gpu)
{
  block_struct *block = psx_gpu->blocks;
  u32 num_blocks = psx_gpu->num_blocks;

  texel_blocks_16bpp += num_blocks;

  vec_8x16u texels;

  u16 *texture_ptr_16bpp = psx_gpu->texture_page_ptr;
  u32 offset;
  u32 i;

  while(num_blocks)
  {
    for(i = 0; i < 8; i++)
    {
      offset = block->uv.e[i];
      offset += ((offset & 0xFF00) * 3);

      texels.e[i] = texture_ptr_16bpp[offset];
    }

    block->texels = texels;

    num_blocks--;
    block++;
  }
}

shade_blocks_textured_modulated_builder(shaded, dithered, direct);
shade_blocks_textured_modulated_builder(shaded, undithered, direct);
shade_blocks_textured_modulated_builder(unshaded, dithered, direct);
shade_blocks_textured_modulated_builder(unshaded, undithered, direct);

shade_blocks_textured_modulated_builder(shaded, dithered, indirect);
shade_blocks_textured_modulated_builder(shaded, undithered, indirect);
shade_blocks_textured_modulated_builder(unshaded, dithered, indirect);
shade_blocks_textured_modulated_builder(unshaded, undithered, indirect);

shade_blocks_textured_unmodulated_builder(indirect)
shade_blocks_textured_unmodulated_builder(direct)

void shade_blocks_unshaded_untextured_indirect(psx_gpu_struct *psx_gpu)
{
}

void shade_blocks_unshaded_untextured_direct(psx_gpu_struct *psx_gpu)
{
  block_struct *block = psx_gpu->blocks;
  u32 num_blocks = psx_gpu->num_blocks;

  vec_8x16u pixels = block->pixels;
  shade_blocks_load_msb_mask_direct();

  while(num_blocks)
  {
    shade_blocks_store_direct(block->draw_mask, pixels);

    num_blocks--;
    block++;
  }
}

void blend_blocks_textured_unblended_off(psx_gpu_struct *psx_gpu)
{
}

blend_blocks_builder(textured, average, off);
blend_blocks_builder(textured, average, on);
blend_blocks_builder(textured, add, off);
blend_blocks_builder(textured, add, on);
blend_blocks_builder(textured, subtract, off);
blend_blocks_builder(textured, subtract, on);
blend_blocks_builder(textured, add_fourth, off);
blend_blocks_builder(textured, add_fourth, on);

blend_blocks_builder(untextured, average, off);
blend_blocks_builder(untextured, average, on);
blend_blocks_builder(untextured, add, off);
blend_blocks_builder(untextured, add, on);
blend_blocks_builder(untextured, subtract, off);
blend_blocks_builder(untextured, subtract, on);
blend_blocks_builder(untextured, add_fourth, off);
blend_blocks_builder(untextured, add_fourth, on);

blend_blocks_builder(textured, unblended, on);

void texture_sprite_blocks_8bpp(psx_gpu_struct *psx_gpu)
{
  block_struct *block = psx_gpu->blocks;
  u32 num_blocks = psx_gpu->num_blocks;

  vec_8x16u texels;
  vec_8x8u texel_indexes;

  u16 *clut_ptr = psx_gpu->clut_ptr;
  u32 i;

  while(num_blocks)
  {
    texel_indexes = block->r;

    for(i = 0; i < 8; i++)
    {
      texels.e[i] = clut_ptr[texel_indexes.e[i]];
    }

    block->texels = texels;

    num_blocks--;
    block++;
  }
}

setup_sprite_tiled_builder(4bpp,);
setup_sprite_tiled_builder(8bpp,);

setup_sprite_tiled_builder(4bpp,_4x);
setup_sprite_tiled_builder(8bpp,_4x);

void setup_sprite_16bpp(psx_gpu_struct *psx_gpu, s32 x, s32 y, s32 u,
 s32 v, s32 width, s32 height, u32 color)
{
  u32 left_offset = u & 0x7;
  u32 width_rounded = width + left_offset + 7;

  u16 *fb_ptr = psx_gpu->vram_out_ptr + (y * 1024) + (s32)(x - left_offset);
  u32 right_width = width_rounded & 0x7;
  u32 block_width = width_rounded / 8;
  u32 fb_ptr_pitch = (1024 + 8) - (block_width * 8);

  u32 left_mask_bits = ~(0xFF << left_offset);
  u32 right_mask_bits = 0xFE << right_width;

  u32 texture_offset_base = u + (v * 1024);
  u32 texture_mask =
   psx_gpu->texture_mask_width | (psx_gpu->texture_mask_height * 1024);

  u32 blocks_remaining;
  u32 num_blocks = psx_gpu->num_blocks;
  block_struct *block = psx_gpu->blocks + num_blocks;

  u16 *texture_page_ptr = psx_gpu->texture_page_ptr;
  u16 *texture_block_ptr;

  texture_offset_base &= ~0x7;

  sprites_16bpp++;

  if(block_width == 1)
  {
    u32 mask_bits = left_mask_bits | right_mask_bits;

    while(height)
    {
      num_blocks++;
      sprite_blocks++;

      if(num_blocks > MAX_BLOCKS)
      {
        flush_render_block_buffer(psx_gpu);
        num_blocks = 1;
        block = psx_gpu->blocks;
      }
      
      texture_block_ptr =
       texture_page_ptr + (texture_offset_base & texture_mask);

      load_128b(block->texels, texture_block_ptr);
      block->draw_mask_bits = mask_bits;
      block->fb_ptr = fb_ptr;

      block++;

      texture_offset_base += 1024;
      fb_ptr += 1024;

      height--;
      psx_gpu->num_blocks = num_blocks;
    }
  }
  else
  {
    u32 texture_offset;

    while(height)
    {
      blocks_remaining = block_width - 2;
      num_blocks += block_width;
      sprite_blocks += block_width;

      if(num_blocks > MAX_BLOCKS)
      {
        flush_render_block_buffer(psx_gpu);
        num_blocks = block_width;
        block = psx_gpu->blocks;
      }

      texture_offset = texture_offset_base;
      texture_offset_base += 1024;

      texture_block_ptr = texture_page_ptr + (texture_offset & texture_mask);
      load_128b(block->texels, texture_block_ptr);

      block->draw_mask_bits = left_mask_bits;
      block->fb_ptr = fb_ptr;

      texture_offset += 8;
      fb_ptr += 8;
      block++;

      while(blocks_remaining)
      {
        texture_block_ptr = texture_page_ptr + (texture_offset & texture_mask);
        load_128b(block->texels, texture_block_ptr);

        block->draw_mask_bits = 0;
        block->fb_ptr = fb_ptr;

        texture_offset += 8;
        fb_ptr += 8;
        block++;

        blocks_remaining--;
      }

      texture_block_ptr = texture_page_ptr + (texture_offset & texture_mask);
      load_128b(block->texels, texture_block_ptr);

      block->draw_mask_bits = right_mask_bits;
      block->fb_ptr = fb_ptr;

      fb_ptr += fb_ptr_pitch;
      block++;

      height--;
      psx_gpu->num_blocks = num_blocks;
    }
  }
}

void setup_sprite_untextured(psx_gpu_struct *psx_gpu, s32 x, s32 y, s32 u,
 s32 v, s32 width, s32 height, u32 color)
{
  if((psx_gpu->render_state & (RENDER_STATE_MASK_EVALUATE |
   RENDER_FLAGS_MODULATE_TEXELS | RENDER_FLAGS_BLEND)) == 0 &&
   (psx_gpu->render_mode & RENDER_INTERLACE_ENABLED) == 0)
  {
    setup_sprite_untextured_simple(psx_gpu, x, y, u, v, width, height, color);
    return;
  }

  u32 right_width = ((width - 1) & 0x7) + 1;
  u32 right_mask_bits = (0xFF << right_width);
  u16 *fb_ptr = psx_gpu->vram_out_ptr + (y * 1024) + x;
  u32 block_width = (width + 7) / 8;
  u32 fb_ptr_pitch = 1024 - ((block_width - 1) * 8);
  u32 blocks_remaining;
  u32 num_blocks = psx_gpu->num_blocks;
  block_struct *block = psx_gpu->blocks + num_blocks;

  u32 color_r = color & 0xFF;
  u32 color_g = (color >> 8) & 0xFF;
  u32 color_b = (color >> 16) & 0xFF;
  vec_8x16u colors;
  vec_8x16u right_mask;
  vec_8x16u test_mask = psx_gpu->test_mask;
  vec_8x16u zero_mask;

  sprites_untextured++;

  color = (color_r >> 3) | ((color_g >> 3) << 5) | ((color_b >> 3) << 10);

  dup_8x16b(colors, color);
  dup_8x16b(zero_mask, 0x00);
  dup_8x16b(right_mask, right_mask_bits);
  tst_8x16b(right_mask, right_mask, test_mask);

  while(height)
  {
    blocks_remaining = block_width - 1;
    num_blocks += block_width;

#ifdef PROFILE
    sprite_blocks += block_width;
#endif

    if(num_blocks > MAX_BLOCKS)
    {
      flush_render_block_buffer(psx_gpu);
      num_blocks = block_width;
      block = psx_gpu->blocks;
    }

    while(blocks_remaining)
    {
      block->pixels = colors;
      block->draw_mask = zero_mask;
      block->fb_ptr = fb_ptr;

      fb_ptr += 8;
      block++;
      blocks_remaining--;
    }

    block->pixels = colors;
    block->draw_mask = right_mask;
    block->fb_ptr = fb_ptr;

    block++;
    fb_ptr += fb_ptr_pitch;

    height--;
    psx_gpu->num_blocks = num_blocks;
  }
}

void setup_sprite_16bpp_4x(psx_gpu_struct *psx_gpu, s32 x, s32 y, s32 u,
 s32 v, s32 width, s32 height, u32 color)
{
  u32 left_offset = u & 0x7;
  u32 width_rounded = width + left_offset + 7;

  u16 *fb_ptr = psx_gpu->vram_out_ptr + (y * 1024) + (s32)(x - left_offset * 2);
  u32 right_width = width_rounded & 0x7;
  u32 block_width = width_rounded / 8;
  u32 fb_ptr_pitch = (2048 + 16) - (block_width * 16);

  u32 left_mask_bits = ~(0xFFFF << (left_offset * 2));
  u32 right_mask_bits = 0xFFFC << (right_width * 2);

  u32 texture_offset_base = u + (v * 1024);
  u32 texture_mask =
   psx_gpu->texture_mask_width | (psx_gpu->texture_mask_height * 1024);

  u32 blocks_remaining;
  u32 num_blocks = psx_gpu->num_blocks;
  block_struct *block = psx_gpu->blocks + num_blocks;

  u16 *texture_page_ptr = psx_gpu->texture_page_ptr;
  u16 *texture_block_ptr;

  texture_offset_base &= ~0x7;

  sprites_16bpp++;

  if(block_width == 1)
  {
    u32 mask_bits = left_mask_bits | right_mask_bits;
    u32 mask_bits_a = mask_bits & 0xFF;
    u32 mask_bits_b = mask_bits >> 8;
    
    vec_8x16u texels;
    vec_8x16u texels_wide;

    while(height)
    {
      num_blocks += 4;
      sprite_blocks += 4;

      if(num_blocks > MAX_BLOCKS)
      {
        flush_render_block_buffer(psx_gpu);
        num_blocks = 4;
        block = psx_gpu->blocks;
      }
      
      texture_block_ptr =
       texture_page_ptr + (texture_offset_base & texture_mask);

      load_128b(texels, texture_block_ptr);
      
      zip_4x32b(vector_cast(vec_4x32u, texels_wide), texels.low, texels.low);
      block->texels = texels_wide;
      block->draw_mask_bits = mask_bits_a;
      block->fb_ptr = fb_ptr;          
      block++;
      
      block->texels = texels_wide;
      block->draw_mask_bits = mask_bits_a;
      block->fb_ptr = fb_ptr + 1024;          
      block++;
      
      zip_4x32b(vector_cast(vec_4x32u, texels_wide), texels.high, texels.high);
      block->texels = texels_wide;
      block->draw_mask_bits = mask_bits_b;
      block->fb_ptr = fb_ptr + 8;
      block++;
      
      block->texels = texels_wide;
      block->draw_mask_bits = mask_bits_b;
      block->fb_ptr = fb_ptr + 8 + 1024;          
      block++;      

      texture_offset_base += 1024;
      fb_ptr += 2048;

      height--;
      psx_gpu->num_blocks = num_blocks;
    }
  }
  else
  {
    u32 texture_offset;
    
    u32 left_mask_bits_a = left_mask_bits & 0xFF;
    u32 left_mask_bits_b = left_mask_bits >> 8;
    u32 right_mask_bits_a = right_mask_bits & 0xFF;
    u32 right_mask_bits_b = right_mask_bits >> 8;
    
    vec_8x16u texels;
    vec_8x16u texels_wide;    

    while(height)
    {
      blocks_remaining = block_width - 2;
      num_blocks += block_width * 4;
      sprite_blocks += block_width * 4;

      if(num_blocks > MAX_BLOCKS)
      {
        flush_render_block_buffer(psx_gpu);
        num_blocks = block_width * 4;
        block = psx_gpu->blocks;
      }

      texture_offset = texture_offset_base;
      texture_offset_base += 1024;

      texture_block_ptr = texture_page_ptr + (texture_offset & texture_mask);
      
      load_128b(texels, texture_block_ptr);

      zip_4x32b(vector_cast(vec_4x32u, texels_wide), texels.low, texels.low);
      block->texels = texels_wide;
      block->draw_mask_bits = left_mask_bits_a;
      block->fb_ptr = fb_ptr;
      block++;
      
      block->texels = texels_wide;
      block->draw_mask_bits = left_mask_bits_a;
      block->fb_ptr = fb_ptr + 1024;
      block++;      

      zip_4x32b(vector_cast(vec_4x32u, texels_wide), texels.high, texels.high);
      block->texels = texels_wide;
      block->draw_mask_bits = left_mask_bits_b;
      block->fb_ptr = fb_ptr + 8;
      block++;  
      
      block->texels = texels_wide;
      block->draw_mask_bits = left_mask_bits_b;
      block->fb_ptr = fb_ptr + 8 + 1024;
      block++;  
      
      texture_offset += 8;
      fb_ptr += 16;

      while(blocks_remaining)
      {
        texture_block_ptr = texture_page_ptr + (texture_offset & texture_mask);
        load_128b(texels, texture_block_ptr);

        zip_4x32b(vector_cast(vec_4x32u, texels_wide), texels.low, texels.low);
        block->texels = texels_wide;
        block->draw_mask_bits = 0;
        block->fb_ptr = fb_ptr;
        block++;
        
        block->texels = texels_wide;
        block->draw_mask_bits = 0;
        block->fb_ptr = fb_ptr + 1024;
        block++;      

        zip_4x32b(vector_cast(vec_4x32u, texels_wide), texels.high, texels.high);
        block->texels = texels_wide;
        block->draw_mask_bits = 0;
        block->fb_ptr = fb_ptr + 8;
        block++;
        
        block->texels = texels_wide;
        block->draw_mask_bits = 0;
        block->fb_ptr = fb_ptr + 8 + 1024;
        block++;
        
        texture_offset += 8;
        fb_ptr += 16;

        blocks_remaining--;
      }

      texture_block_ptr = texture_page_ptr + (texture_offset & texture_mask);
      load_128b(texels, texture_block_ptr);
      
      zip_4x32b(vector_cast(vec_4x32u, texels_wide), texels.low, texels.low);
      block->texels = texels_wide;
      block->draw_mask_bits = right_mask_bits_a;
      block->fb_ptr = fb_ptr;
      block++;
      
      block->texels = texels_wide;
      block->draw_mask_bits = right_mask_bits_a;
      block->fb_ptr = fb_ptr + 1024;
      block++;      

      zip_4x32b(vector_cast(vec_4x32u, texels_wide), texels.high, texels.high);
      block->texels = texels_wide;
      block->draw_mask_bits = right_mask_bits_b;
      block->fb_ptr = fb_ptr + 8;
      block++;

      block->texels = texels_wide;
      block->draw_mask_bits = right_mask_bits_b;
      block->fb_ptr = fb_ptr + 8 + 1024;      
      block++;

      fb_ptr += fb_ptr_pitch;

      height--;
      psx_gpu->num_blocks = num_blocks;
    }
  }
}

void scale2x_tiles8(void *dst, const void *src, int w8, int h)
{
   // TODO?
}
