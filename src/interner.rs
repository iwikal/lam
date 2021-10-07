use std::collections::HashMap;
use std::mem;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct InternedHandle {
    index: u32,
}

// SAFETY these static lifetimes aren't actually static!
// They must never be leaked through the public API.
#[derive(Default)]
pub struct Interner {
    map: HashMap<&'static str, InternedHandle>,
    vec: Vec<&'static str>,
    buffer: String,
    full_buffers: Vec<Box<str>>,
}

impl std::fmt::Debug for Interner {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Interner {{ ... }}")
    }
}

impl Interner {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn intern(&mut self, name: &str) -> InternedHandle {
        if let Some(&handle) = self.map.get(name) {
            return handle;
        }

        let name = unsafe { self.alloc(name) };
        let index = self.map.len() as u32;
        let handle = InternedHandle { index };
        self.map.insert(name, handle);
        self.vec.push(name);
        handle
    }

    pub fn lookup(&self, handle: InternedHandle) -> &str {
        self.vec[handle.index as usize]
    }

    unsafe fn alloc(&mut self, name: &str) -> &'static str {
        let new_len = self.buffer.len() + name.len();

        if self.buffer.capacity() < new_len {
            let new_buffer = String::with_capacity(new_len.next_power_of_two());
            let old_buf = mem::replace(&mut self.buffer, new_buffer);
            let old_buf = old_buf.into_boxed_str();
            self.full_buffers.push(old_buf);
        }

        let interned = {
            let start = self.buffer.len();
            self.buffer.push_str(name);
            &self.buffer[start..] as *const str
        };

        // SAFETY: the old buffers are never changed.
        &*interned
    }
}
