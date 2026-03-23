use std::{
    error::Error as StdError,
    fmt::{self, Display},
};

use serde::ser::{
    self, Serialize, SerializeMap, SerializeSeq, SerializeStruct, SerializeStructVariant,
    SerializeTuple, SerializeTupleStruct, SerializeTupleVariant, Serializer,
};

/// the demo struct
struct Test {
    a: i64,
    b: String,
}

impl Serialize for Test {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut ss = serializer.serialize_struct("test", 2)?;
        //ss.serialize_field("a", &self.a)?; // method 1
        // or other way method 2
        serde::ser::SerializeStruct::serialize_field(&mut ss, "a", &self.a)?;
        ss.serialize_field("b", &self.b)?;
        ss.end()
    }
}

// custom serializer
#[derive(Debug)]
pub struct MyError;

impl fmt::Display for MyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl StdError for MyError {}

impl serde::ser::Error for MyError {
    #[doc = r" Used when a [`Serialize`] implementation encounters any error"]
    #[doc = r" while serializing a type."]
    #[doc = r""]
    #[doc = r" The message should not be capitalized and should not end with a"]
    #[doc = r" period."]
    #[doc = r""]
    #[doc = r" For example, a filesystem [`Path`] may refuse to serialize"]
    #[doc = r" itself if it contains invalid UTF-8 data."]
    #[doc = r""]
    #[doc = r" `edition2021`"]
    #[doc = r""]
    #[doc = r" [`Path`]: std::path::Path"]
    #[doc = r" [`Serialize`]: crate::Serialize"]
    fn custom<T>(msg: T) -> Self
    where
        T: Display,
    {
        todo!()
    }
}

pub struct MySerializer {
    pub output: String,
}

impl<'a> SerializeSeq for &'a mut MySerializer {
    type Ok = ();
    type Error = MyError;
    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        value.serialize(&mut **self)?;
        self.output.push(',');
        Ok(())
    }
    fn end(self) -> Result<(), Self::Error> {
        Ok(())
    }
}

// For fixed-size tuples: (1, "a")
impl<'a> SerializeTuple for &'a mut MySerializer {
    type Ok = ();
    type Error = MyError;
    fn serialize_element<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        value.serialize(&mut **self)?;
        Ok(())
    }
    fn end(self) -> Result<(), Self::Error> {
        Ok(())
    }
}

// For Tuple Structs: struct Color(u8, u8, u8)
impl<'a> SerializeTupleStruct for &'a mut MySerializer {
    type Ok = ();
    type Error = MyError;
    fn serialize_field<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        value.serialize(&mut **self)?;
        Ok(())
    }
    fn end(self) -> Result<(), Self::Error> {
        Ok(())
    }
}

// For Maps: {"key": "value"}
impl<'a> SerializeMap for &'a mut MySerializer {
    type Ok = ();
    type Error = MyError;
    fn serialize_key<T: ?Sized + Serialize>(&mut self, key: &T) -> Result<(), Self::Error> {
        key.serialize(&mut **self)?;
        self.output.push(':');
        Ok(())
    }
    fn serialize_value<T: ?Sized + Serialize>(&mut self, value: &T) -> Result<(), Self::Error> {
        value.serialize(&mut **self)?;
        self.output.push(',');
        Ok(())
    }
    fn end(self) -> Result<(), Self::Error> {
        Ok(())
    }
}

// For Enum Variants (Tuples and Structs)
impl<'a> SerializeTupleVariant for &'a mut MySerializer {
    type Ok = ();
    type Error = MyError;
    fn serialize_field<T: ?Sized + Serialize>(&mut self, _v: &T) -> Result<(), Self::Error> {
        Ok(())
    }
    fn end(self) -> Result<(), Self::Error> {
        Ok(())
    }
}

impl<'a> SerializeStructVariant for &'a mut MySerializer {
    type Ok = ();
    type Error = MyError;
    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        _k: &'static str,
        _v: &T,
    ) -> Result<(), Self::Error> {
        Ok(())
    }
    fn end(self) -> Result<(), Self::Error> {
        Ok(())
    }
}

impl<'a> ser::Serializer for &'a mut MySerializer {
    type Ok = ();
    type Error = MyError;

    type SerializeSeq = Self;
    type SerializeTuple = Self;
    type SerializeTupleStruct = Self;
    type SerializeTupleVariant = Self;
    type SerializeMap = Self;
    type SerializeStruct = Self;
    type SerializeStructVariant = Self;

    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        self.output.push_str(&v.to_string());
        Ok(())
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        self.output.push_str(v);
        Ok(())
    }

    // start to serial struct
    fn serialize_struct(
        self,
        name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        self.output.push_str(name);
        self.output.push('{');
        Ok(self)
    }

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        todo!()
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_unit_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        todo!()
    }

    fn serialize_newtype_struct<T>(
        self,
        name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        todo!()
    }

    fn serialize_newtype_variant<T>(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Serialize,
    {
        todo!()
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        todo!()
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        todo!()
    }

    fn serialize_tuple_struct(
        self,
        name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        todo!()
    }

    fn serialize_tuple_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        todo!()
    }

    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        todo!()
    }

    fn serialize_struct_variant(
        self,
        name: &'static str,
        variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        todo!()
    }

    fn serialize_i128(self, v: i128) -> Result<Self::Ok, Self::Error> {
        let _ = v;
        Err(ser::Error::custom("i128 is not supported"))
    }

    fn serialize_u128(self, v: u128) -> Result<Self::Ok, Self::Error> {
        let _ = v;
        Err(ser::Error::custom("u128 is not supported"))
    }

    fn is_human_readable(&self) -> bool {
        true
    }

    fn collect_seq<I>(self, iter: I) -> Result<Self::Ok, Self::Error>
    where
        I: IntoIterator,
        <I as IntoIterator>::Item: Serialize,
    {
        todo!()
    }

    fn collect_map<K, V, I>(self, iter: I) -> Result<Self::Ok, Self::Error>
    where
        K: Serialize,
        V: Serialize,
        I: IntoIterator<Item = (K, V)>,
    {
        todo!()
    }

    fn collect_str<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + Display,
    {
        self.serialize_str(&value.to_string())
    }
}

impl<'a> ser::SerializeStruct for &'a mut MySerializer {
    type Ok = ();
    type Error = MyError;

    fn serialize_field<T: ?Sized + Serialize>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        self.output.push_str(key);
        self.output.push(':');
        value.serialize(&mut **self)?;
        self.output.push(',');
        Ok(())
    }

    fn end(self) -> Result<(), Self::Error> {
        self.output.push('}');
        Ok(())
    }
}

fn main() {
    let mut my_ser = MySerializer {
        output: String::new(),
    };
    let a = Test {
        a: 1,
        b: "bbb".into(),
    };

    // The call you were looking for:
    a.serialize(&mut my_ser).unwrap();

    println!("Custom Format: {}", my_ser.output);
}
