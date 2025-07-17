/*
 * Copyright © 2024 wysiwys
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *
 */

#![allow(warnings)]

use criterion::{black_box, criterion_group, criterion_main, Criterion};

mod common;
use common::*;

use xkbcommon_rs::xkb_keymap::RuleNames;
use xkbcommon_rs::*;

fn rulescomp_c(
    rules: &str,
    model: &str,
    layout: &str,
    variant: &str,
    options: Option<String>,
    ctx: &xkbcommon::xkb::Context,
) {
    xkbcommon::xkb::Keymap::new_from_names(ctx, rules, model, layout, variant, options, 0).unwrap();
}
fn rulescomp_rust(rmlvo: Option<RuleNames>, ctx: Box<Context>) {
    Keymap::new_from_names(*ctx, rmlvo, 0).unwrap();
}

fn criterion_benchmark(c: &mut Criterion) {
    let rules = "evdev";
    let model = "evdev";
    let layout = "us";
    let variant = "";

    let rmlvo = RuleNames::new(rules, model, layout, variant, "");
    let mut context = test_get_context(TestContextFlags::empty()).unwrap();
    context.set_log_verbosity(0);
    let context = Box::new(context);

    c.bench_function("rulescomp_rust", |b| {
        b.iter(|| rulescomp_rust(black_box(Some(rmlvo.clone())), black_box(context.clone())))
    });

    let mut context = xkbcommon::xkb::Context::new(0);

    c.bench_function("rulescomp_c", |b| {
        b.iter(|| {
            rulescomp_c(
                black_box(&rules),
                black_box(&model),
                black_box(&layout),
                black_box(&variant),
                black_box(None),
                black_box(&context),
            )
        })
    });
}

criterion_group!(benches, criterion_benchmark);

criterion_main!(benches);
