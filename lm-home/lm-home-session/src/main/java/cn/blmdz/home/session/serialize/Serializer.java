package cn.blmdz.home.session.serialize;

import java.util.Map;

import cn.blmdz.home.session.exception.SerializeException;

public interface Serializer {
   String serialize(Object var1);

   Map deserialize(String var1) throws SerializeException;
}
