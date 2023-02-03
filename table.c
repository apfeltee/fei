
#include "fei.h"

static inline FeiValTabEntry* fei_valtable_internfindentry(FeiState* state, int count, FeiValTabEntry* entries, int capacity, FeiString* key)
{
    uint32_t index;
    FeiValTabEntry* entry;
    FeiValTabEntry* tombstone;
    (void)state;
    (void)count;
    // use modulo to map the key's hash to the code index
    index = key->hash % capacity;
    tombstone = NULL;
    while(true)
    {
        // index is 'inserted' here
        entry = &entries[index];
        if(entry->key == NULL)
        {
            if(fei_value_isnull(entry->value))
            {
                // empty entry
                if(tombstone != NULL)
                {
                    return tombstone;
                }
                return entry;
            }
            else
            {
                if(tombstone == NULL)
                {
                    // can return tombstone bucket as empty and reuse it
                    tombstone = entry;
                }
            }
        }
        // compare them in MEMORY
        if(entry->key == key)
        {
            return entry;
        }
        index = (index + 1) % capacity;
    }
    return NULL;
}

FeiValTable* fei_valtable_make(FeiState* state, int cap)
{
    FeiValTable* table;
    table = (FeiValTable*)ALLOCATE(state, sizeof(FeiValTable), 1);
    fei_valtable_initnull(state, table);
    if(cap > 0)
    {
        fei_valtable_adjustcapacity(state, table, cap);
    }
    return table;
}

void fei_valtable_initnull(FeiState* state, FeiValTable* table)
{
    (void)state;
    table->count = 0;
    table->capacity = 0;
    table->entries = NULL;
}

void fei_valtable_destroy(FeiState* state, FeiValTable* table)
{
    FREE_ARRAY(state, sizeof(FeiValTabEntry), table->entries, table->capacity);
    //fei_valtable_initnull(state, table);
    FREE(state, sizeof(FeiValTable), table);
}

bool fei_valtable_get(FeiState* state, FeiValTable* table, FeiString* key, FeiValue* value)
{
    FeiValTabEntry* entry;
    if(table->count == 0)
    {
        return false;
    }
    entry = fei_valtable_internfindentry(state, table->count, table->entries, table->capacity, key);
    if(entry == NULL)
    {
        return false;
    }
    if(entry->key == NULL)
    {
        return false;
    }
    *value = entry->value;
    return true;
}

void fei_valtable_adjustcapacity(FeiState* state, FeiValTable* table, int capacity)
{
    int i;
    int actualcap;
    FeiValTabEntry* dest;
    FeiValTabEntry* entry;
    FeiValTabEntry* entries;
    actualcap = capacity+2;
    entries = (FeiValTabEntry*)ALLOCATE(state, sizeof(FeiValTabEntry), actualcap);
    for(i = 0; i < capacity; i++)
    {
        entries[i].key = NULL;
        entries[i].value = fei_value_makenull(state);
    }
    // do not copy tombstones over when growing
    // NOTE: entries may end up in different buckets
    // with the same hash as it is divided by the modulo; loop below recalculates everything
    //fprintf(stderr, "fei_valtable_adjustcapacity(%d): table->count=%d table->entries=%p\n", actualcap, table->count, table->entries);
    if((table->count > 0) && (table->entries != NULL))
    {
        table->count = 0;
        // traverse through old array
        for(i = 0; i < table->capacity; i++)
        {
            entry = &table->entries[i];
            if(entry->key == NULL)
            {
                continue;
            }
            // pass in new array
            dest = fei_valtable_internfindentry(state, table->count, entries, actualcap, entry->key);
            dest->key = entry->key;
            dest->value = entry->value;
            table->count++;
        }
        FREE_ARRAY(state, sizeof(FeiValTabEntry), table->entries, table->capacity);
    }
    table->entries = entries;
    table->capacity = actualcap;
}

// inserting into the table, return false if collision
bool fei_valtable_set(FeiState* state, FeiValTable* table, FeiString* key, FeiValue value)
{
    bool isnewkey;
    int capacity;
    FeiValTabEntry* entry;
    // make sure array is big enough
    if((table->count + 1) > (table->capacity * TABLE_MAX_LOAD))
    {
        capacity = (GROW_CAPACITY(table->capacity));
        fei_valtable_adjustcapacity(state, table, capacity);
    }
    entry = fei_valtable_internfindentry(state, table->count, table->entries, table->capacity, key);
    isnewkey = entry->key == NULL;
    if(isnewkey && fei_value_isnull(entry->value))
    {
        // fei_value_isnull for tombstones; treat them as full objects
        table->count++;
    }
    entry->key = key;
    entry->value = value;
    return isnewkey;
}

bool fei_valtable_delete(FeiState* state, FeiValTable* table, FeiString* key)
{
    FeiValTabEntry* entry;
    if(table->count == 0)
    {
        return false;
    }
    // find entry
    entry = fei_valtable_internfindentry(state, table->count, table->entries, table->capacity, key);
    if(entry->key == NULL)
    {
        return false;
    }
    // place tombstone
    entry->key = NULL;
    //bool(true) as the tombstone
    entry->value = fei_value_makebool(state, true);
    return true;
}

void fei_valtable_mergefrom(FeiState* state, FeiValTable* from, FeiValTable* to)
{
    int i;
    FeiValTabEntry* entry;
    for(i = 0; i < from->capacity; i++)
    {
        entry = &from->entries[i];
        if(entry->key != NULL)
        {
            fei_valtable_set(state, to, entry->key, entry->value);
        }
    }
}

// used in VM to find the string
// pass in raw character array
FeiString* fei_valtable_findstring(FeiState* state, FeiValTable* table, const char* chars, int length, uint32_t hash)
{
    bool found;
    uint32_t index;
    FeiValTabEntry* entry;
    (void)state;
    if(table->count == 0)
    {
        return NULL;
    }
    index = hash % table->capacity;
    for(;;)
    {
        entry = &table->entries[index];
        if(entry->key == NULL)
        {
            // stop if found empty non-tombstone entry
            if(fei_value_isnull(entry->value))
            {
                // return null if not tombstone(tombstone value is bool(true))
                return NULL;
            }
        }
        else
        {
            found = (
                (entry->key->length == length) &&
                (entry->key->hash == hash) //&&
                //(memcmp(entry->key->chars, chars, length) == 0)
            );
            if(found)
            {
                // found the entry
                return entry->key;
            }
        }
        index = (index + 1) % table->capacity;
    }
    return NULL;
}

// removing unreachable pointers, used to remove string interns in garbage collection
void fei_valtable_removeunreachable(FeiState* state, FeiValTable* table)
{
    int i;
    FeiValTabEntry* entry;
    for(i = 0; i < table->capacity; i++)
    {
        entry = &table->entries[i];
        // remove not marked (string) object pointers
        if(entry->key != NULL && !entry->key->obj.ismarked)
        {
            fei_valtable_delete(state, table, entry->key);
        }
    }
}

// mark global variables, used in VM for garbage collection
void fei_valtable_mark(FeiState* state, FeiValTable* table)
{
    int i;
    FeiValTabEntry* entry;
    for(i = 0; i < table->capacity; i++)
    {
        entry = &table->entries[i];
        // need to mark both the STRING KEYS and the actual value/obj itself
        // mark the string key(FeiString type)
        fei_gcmem_markobject(state, (FeiObject*)entry->key);
        // mark the actual avlue
        fei_gcmem_markvalue(state, entry->value);
    }
}


FeiTable* fei_table_make(FeiState* state)
{
    FeiTable* arr;
    state->ocount.cnttable++;
    arr = (FeiTable*)fei_object_allocobject(state, sizeof(FeiTable), OBJ_TABLE);
    arr->state = state;
    arr->table = fei_valtable_make(state, 0);
    return arr;
}

bool fei_table_destroy(FeiTable* table)
{
    FeiState* state;
    if(table != NULL)
    {
        state = table->state;
        fei_valtable_destroy(state, table->table);
        fei_gcmem_reallocate(state, table, sizeof(FeiTable), 0);
        return true;
    }
    return false;
}

size_t fei_table_count(FeiTable* table)
{
    return table->table->count;
}

bool fei_table_get(FeiTable* table, FeiString* key, FeiValue* value)
{
    return fei_valtable_get(table->state, table->table, key, value);
}

bool fei_table_set(FeiTable* table, FeiString* key, FeiValue value)
{
    return fei_valtable_set(table->state, table->table, key, value);
}

