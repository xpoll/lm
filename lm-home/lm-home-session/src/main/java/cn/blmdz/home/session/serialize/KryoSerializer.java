package cn.blmdz.home.session.serialize;

import cn.blmdz.home.session.exception.SerializeException;
import cn.blmdz.home.session.serialize.Serializer;
import cn.blmdz.home.session.util.KryoUtils;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.io.ByteBufferInput;
import com.esotericsoftware.kryo.io.ByteBufferOutput;
import com.esotericsoftware.kryo.io.Input;
import com.esotericsoftware.kryo.io.Output;
import com.google.common.io.BaseEncoding;

import java.util.Map;

public class KryoSerializer implements Serializer {
   public String serialize(Object o) {
      Kryo kryo = KryoUtils.getKryo();
      Output output = new ByteBufferOutput(4096, 'ꀀ');
      kryo.writeClassAndObject(output, o);
      return BaseEncoding.base16().encode(output.toBytes());
   }

   public Map deserialize(String o) throws SerializeException {
      Kryo kryo = KryoUtils.getKryo();
      Input input = new ByteBufferInput(BaseEncoding.base16().decode(o));
      return (Map)kryo.readClassAndObject(input);
   }
}
