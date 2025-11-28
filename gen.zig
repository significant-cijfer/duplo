fn meaning() i32 {
  const Block = enum {
    b0,
  };
  return block: switch (Block.b0) {
    .b0 => {
      const t0 = 42;
      break :block t0;
    },
  };
}

fn inc(x: i32) i32 {
  const Block = enum {
    b1,
  };
  return block: switch (Block.b1) {
    .b1 => {
      const t4 = x;
      const t6 = 1;
      const t8 = t4 + t6;
      break :block t8;
    },
  };
}

fn f(x: i32) i32 {
  const Block = enum {
    b2,
  };
  return block: switch (Block.b2) {
    .b2 => {
      const t11 = x;
      const t13 = 2;
      const t15 = t11 * t13;
      const y = t15;
      const t17 = x;
      const t19 = y;
      const t21 = t17 + t19;
      break :block t21;
    },
  };
}
